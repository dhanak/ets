#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <iconv.h>
#include <mysql/mysql.h>
#include <sicstus/sicstus.h>

typedef struct mys_handle {
  MYSQL   mh;
  iconv_t ih_s2p, ih_p2s;
} mys_handle;

typedef struct mys_result {
  unsigned int num_fields;
  MYSQL_FIELD *fields;
  MYSQL_RES   *rows;
  iconv_t      ih;
} mys_result;

static void exception(const char *pred, const char *msg)
{
  SP_term_ref args[3];
  SP_term_ref ex;

  args[0] = SP_new_term_ref();
  args[1] = SP_new_term_ref();
  ex      = SP_new_term_ref();
  args[2] = 0;

  SP_put_string(args[0], pred);
  SP_put_string(args[1], msg);

  SP_read_from_string(ex, "mysql_error(Pred, Msg).", args);
  SP_raise_exception(ex);
}

static const char *convert(iconv_t ih, const char *str)
{
  static char convbuf[1024];

  char *inptr = (char *)str;
  char *outptr = convbuf;
  size_t insize, outsize;

  insize = strlen(str);
  outsize = 1023;

  if (iconv(ih, &inptr, &insize, &outptr, &outsize) == (size_t) -1)
    return NULL;

  *outptr = '\0';
  return convbuf;
}

mys_handle *mys_connect(const char *host, const char *user, const char *passwd,
                        const char *db)
{
  mys_handle *handle;

  handle = (mys_handle *)SP_malloc(sizeof(mys_handle));
  mysql_init(&handle->mh);

  if (mysql_real_connect(&handle->mh, host, user, passwd, db, 0, NULL, 0) == NULL) {
    exception("mysql_connect/5", mysql_error(&handle->mh));
    goto mc_mrc_error;
  }

  handle->ih_s2p = iconv_open("UTF-8", mysql_character_set_name(&handle->mh));
  if (handle->ih_s2p == (iconv_t) -1) {
    exception("mysql_connect/5", strerror(errno));
    goto mc_io1_error;
  }

  handle->ih_p2s = iconv_open(mysql_character_set_name(&handle->mh), "UTF-8");
  if (handle->ih_p2s == (iconv_t) -1) {
    exception("mysql_connect/5", strerror(errno));
    goto mc_io2_error;
  }

  return handle;

  /* cleanup code */
 mc_io2_error:
  iconv_close(handle->ih_s2p);
 mc_io1_error:
  mysql_close(&handle->mh);
 mc_mrc_error:
  SP_free(handle);
  return NULL;
}

void mys_close(mys_handle *handle)
{
  iconv_close(handle->ih_p2s);
  iconv_close(handle->ih_s2p);
  mysql_close(&handle->mh);
  SP_free(handle);
}

mys_result *mys_query(mys_handle *handle, const char *query)
{
  mys_result *result;
  unsigned int num_fields;
  const char *sql_query;

  if ((sql_query = convert(handle->ih_p2s, query)) == NULL) {
    exception("mysql_query/3", strerror(errno));
    return NULL;
  }

  if (mysql_query(&handle->mh, sql_query) != 0) {
    /* query failed */
    exception("mysql_query/3", mysql_error(&handle->mh));
    return NULL;
  }

  /* query succeeded, check for return value */
  num_fields = mysql_field_count(&handle->mh);
  if (num_fields == 0)
    return NULL;

  /* fetch results */
  result = SP_malloc(sizeof(mys_result));
  result->ih = handle->ih_s2p;
  result->num_fields = num_fields;
  result->rows = mysql_store_result(&handle->mh);
  if (result->rows == NULL) {
    /* an error occured during fetch */
    exception("mysql_query/3", mysql_error(&handle->mh));
    SP_free(result);
    return NULL;
  }

  /* fetch column descriptions */
  result->fields = mysql_fetch_fields(result->rows);
  return result;
}

void mys_end_query(mys_result *result)
{
  if (result) {
    mysql_free_result(result->rows);
    SP_free(result);
  }
}

void mys_fetch_row(mys_result *result, SP_term_ref row)
{
  MYSQL_ROW sqlrow;
  SP_term_ref head, name, value;
  unsigned int i;

  sqlrow = mysql_fetch_row(result->rows);
  if (sqlrow == NULL) {
    /* no more rows to fetch */
    SP_fail();
    return;
  }

  head  = SP_new_term_ref();
  name  = SP_new_term_ref();
  value = SP_new_term_ref();

  /* build the list from back to forward */
  i = result->num_fields;
  while(i--) {
    SP_put_string(name, result->fields[i].name);
    if (sqlrow[i] == NULL) {	/* null value, return as [] */
      SP_put_string(value, "[]");
    } else if (IS_NUM(result->fields[i].type))
      SP_put_number_codes(value, sqlrow[i]); /* numerical field */
    else {			/* string field, needs conversion */
      const char *str;
      if ((str = convert(result->ih, sqlrow[i])) == NULL) {
        exception("mysql_fetch_row/2", strerror(errno));
        return;
      }
      SP_put_string(value, str);
    }
    SP_cons_functor(head, SP_atom_from_string("-"), 2, name, value);
    SP_cons_list(row, head, row);
  }
}

long mys_affected_rows(mys_handle *handle)
{
  return mysql_affected_rows(&handle->mh);
}
