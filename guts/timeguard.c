#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

typedef enum {
  ERR_NOERR = 0,
  ERR_USAGE,
  ERR_ARGV,
  ERR_INVVAL,
  ERR_FORK,
  ERR_EXEC
} error_t;

void check_result(int status)
{
  if (WIFEXITED(status)) {
    exit(WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    int termsig;
    if ((termsig = WTERMSIG(status)) == SIGKILL)
      termsig = SIGXCPU;
    exit(termsig + 128);
  } else {
    exit(ERR_NOERR);
  }
}

void normal_exit(int)
{
  int status;

  waitpid(-1, &status, WNOHANG);
  check_result(status);
}

int main(int argc, char *argv[])
{
  unsigned int limit, mult;
  struct rlimit rlim;
  int status;
  pid_t pid;

  if (argc < 4) {
    printf("Usage: timeguard <time limit> <realtime multiplier> <program> [args]\n");
    exit(ERR_USAGE);
  }

  if (argv[argc] != NULL) {
    fprintf(stderr, "argv[] doesn't end in NULL.\n");
    exit(ERR_ARGV);
  }

  if ((limit = atoi(argv[1])) == 0) {
    fprintf(stderr, "Time limit must be a positive integer value!\n");
    exit(ERR_INVVAL);
  }
  mult = atoi(argv[2]);
  if (mult)
    signal(SIGCHLD, normal_exit); /* we will sleep and must be woken */

  pid = fork();
  if (pid < 0) {
    fprintf(stderr, "fork() failed.\n");
    exit(ERR_FORK);
  } else if (pid == 0) {
    rlim.rlim_cur = rlim.rlim_max = 0; /* core limit */
    if (setrlimit(RLIMIT_CORE, &rlim))
      fprintf(stderr, "Failed to disable core dumps.\n");

    rlim.rlim_cur = rlim.rlim_max = limit; /* time limit */
    if (setrlimit(RLIMIT_CPU, &rlim))
      fprintf(stderr, "Failed to set CPU time limit.\n");

    rlim.rlim_cur = rlim.rlim_max = 100 * 1024 * 1024; /* 100M file size limit */
    if (setrlimit(RLIMIT_FSIZE, &rlim))
      fprintf(stderr, "Failed to set file size limit.\n");

    rlim.rlim_cur = rlim.rlim_max = 0xc0000000; /* 3000M memory limit */
    if (setrlimit(RLIMIT_AS, &rlim))
      fprintf(stderr, "Failed to set memory limit.\n");

    if (execvp(argv[3], argv+3) != 0) {
      fprintf(stderr, "exec() failed.\n");
      exit(ERR_EXEC);
    }
  }
  if (mult) {
    sleep(mult*limit);		/* SIGCHLD wakes us */
  } else {
    waitpid(pid, &status, 0);
    check_result(status);
  }

  signal(SIGCHLD, SIG_DFL);	/* real time's up! */
  kill(pid, SIGTERM);
  exit(SIGXCPU + 128);

  return 0;
}
