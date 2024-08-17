# Elektronikus Tanársegéd (ETS)

Ez a BME műszaki informatika szakon oktatott, Deklaratív Programozás című tárgy
hallgatói információs rendszere, az ETS.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Tartalom**

- [Elektronikus Tanársegéd (ETS)](#elektronikus-tanársegéd-ets)
    - [Előzetes követelmények](#előzetes-követelmények)
    - [Telepítés](#telepítés)
        - [Futtatási környezet beállítása](#futtatási-környezet-beállítása)
        - [Docker build és futtatás](#docker-build-és-futtatás)
        - [Apache2 reverse proxy beállítása](#apache2-reverse-proxy-beállítása)
    - [Backup és visszaállítás](#backup-és-visszaállítás)
    - [Emailek a fejlesztői környezetben](#emailek-a-fejlesztői-környezetben)
    - [Keycloak szerver a fejlesztői környezetben](#keycloak-szerver-a-fejlesztői-környezetben)
        - [Kezdeti keycloak beállítások](#kezdeti-keycloak-beállítások)

<!-- markdown-toc end -->

## Előzetes követelmények

* `docker`, `docker compose`
  * Debian/Ubuntu: `docker-ce`, `docker-ce-cli`, `docker-compose-plugin`
    csomagok
  * a régebbi, v1-es `docker-compose` python csomag nem jó!
* Apache2 web szerver konfigurált HTTPS eléréssel
* engedélyezett OpenID Connect Provider szolgáltatás (pl. <https://idp.bme.hu>)
* érvényes SICStus Prolog licenc: a licenc paramétereit a `SICSTUS_SITENAME`,
  `SICSTUS_LICENSE_CODE` és `SICSTUS_EXPIRATION_DATE` környezeti változókban
  kell beállítani

## Telepítés

A telepítéshez és futtatáshoz `docker` szükséges. Lépések:

### Futtatási környezet beállítása

Fejlesztői környezet futtatásához használhatod a meglévő beállításokat. Éles
futtatáshoz végezd el a következő lépéseket:

1. Másold le az `env.develop` file-t `env.live` néven!

2. A benne szereplő változóknak adj a célnak megfelelő értéket!

3. `ln -s env.live .env`

### Docker build és futtatás

```sh
$ docker compose build
$ docker compose up -d
```

Ezek hatására a site fut, és elérhető a docker hoston a beállított porton
(`HTTP_EXTERNAL_PORT`, alapértelmezetten 8080).

### Apache2 reverse proxy beállítása

Az ETS webszerver SSL titkosítás nélkül, csak a localhoston hozzáférhető porton
(alapértelmezetten a 8080-ason, ld. `HTTP_EXTERNAL_PORT` beállítást az env
file-ban) fut. Ahhoz, hogy kívülről is elérhető legyen https protokollal, be
kell rakni egy futó Apache webszerver mögé, amiben beállítottuk az SSL elérést
és reverse proxy-s továbbítást. Ehhez először is szükség lesz a `mod_proxy` és a
`mod_headers` Apache modulokra:

```sh
$ sudo a2enmod headers
$ sudo a2enmod proxy
```

Továbbá az alábbi file-ra `/etc/apache2/sites-avaiable/ets-ssl.conf` néven. Az
ETS szempontjából lényeges részek az `## ETS` és `## ETS END` sorok között
vannak, a többi csak példa, és feltételezi a `SERVER_HOST` környezeti változó
beállítását arra az FQDN névre, amin a host kívülről elérhető (ez persze fixen
ki bele is írható a file-ba a `${SERVER_HOST}` előfordulásainak helyére).

```conf
<VirtualHost *:443>
    ServerAdmin webmaster@localhost
    ServerName ${SERVER_HOST}

    DocumentRoot /var/www/html

    ErrorLog ${APACHE_LOG_DIR}/error.log
    CustomLog ${APACHE_LOG_DIR}/access.log combined

    SSLEngine on

    <FilesMatch "\.(?:cgi|shtml|phtml|php)$">
        SSLOptions +StdEnvVars
    </FilesMatch>
    <Directory /usr/lib/cgi-bin>
        SSLOptions +StdEnvVars
    </Directory>

    ## ETS
    SSLProxyEngine on
    SSLProxyVerify none
    SSLProxyCheckPeerCN off
    ProxyPass /ets/redirect_uri http://localhost:8080/ets/redirect_uri
    ProxyPass /ets http://localhost:8080
    ProxyPassReverse /ets http://localhost:8080
    ProxyPreserveHost on
    RequestHeader set X-Forwarded-Proto "https"
    RequestHeader set X-Forwarded-Port 443
    ## ETS END

    SSLCertificateFile /etc/letsencrypt/live/${SERVER_HOST}/fullchain.pem
    SSLCertificateKeyFile /etc/letsencrypt/live/${SERVER_HOST}/privkey.pem
    Include /etc/letsencrypt/options-ssl-apache.conf
</VirtualHost>
```

Végezetül engedélyezzük a frissen létrehozott site-ot:

```sh
$ sudo a2ensite ets-ssl
```

## Backup és visszaállítás

Előfeltétel: fut a container. Az alábbi parancsokat a repository gyökerében kell
futtatni.

Teljes DB backup:

```sh
$ docker compose exec mariadb mariadb-dump -uets -pets ets | \
    gzip > ets-db-backup-$(date +%Y%m%d).sql.gz
```

Visszaállítás:

```sh
$ gunzip -c ets-db-backup-....sql.gz | \
    docker compose exec -T mariadb mariadb -uets -pets ets
```

## Emailek a fejlesztői környezetben

Fejlesztői környezetben a rendszer nem küld éles emaileket, hanem egy
[MailCatcher](https://mailcatcher.me/) nevű beágyazott SMTP szerveren gyűjti
össze azokat. A kiküldött emaileket egy webes felületen keresztül lehet
megtekinteni a docker host gépen, a 1080-as porton:

```
$ xdg-open http://localhost:1080
```

## Keycloak szerver a fejlesztői környezetben

Az OpenID Connect azonosításhoz fejlesztői környezetben egy
[keycloak](https://www.keycloak.org/) szervert futtatunk. Ebben
alapértelmezésben be van állítva egy `ets` realm, két felhasználóval:

* admin/admin (Neptun kód: ADMIN1);
* jakab/jakab (Neptun kód: JAKAB1).

Az ETS saját adatbázisába alapértelmezésben ADMIN1 fel van véve
adminisztrátorként, így vele rögtön be tudunk lépni. JAKAB1 nincs, ezért őt
ismeretlen (vendég) felhasználóként mutatja a rendszer. Ahhoz, hogy teljes jogú
felhasználó legyen, az ETS adatbázisába is fel kell venni a felhasználók közé a
JAKAB1 Neptun kódot.

További felhasználók rögzíthetők a keycloak belépési oldalán a Register linkre
klikkelve, vagy a keycloak administrációs oldalán a <http://localhost:8081>
címen. (A külső port állítható az `OIDC_EXTERNAL_PORT` környezeti változóval.)

### Kezdeti keycloak beállítások

A kezdeti beállításokat (realm, admin és jakab felhasználók) az `ets-realm.json`
file tartalmazza. Ennek létrehozása nem teljesen magától értetődő, a következő
technikát értemes használni:

```
$ docker compose run -it --rm --name keycloak-export --service-ports \
    --entrypoint /bin/bash keycloak

bash-5.1$ /opt/keycloak/bin/kc.sh start-dev --import-realm
...
2024-08-14 08:39:14,281 WARN  [org.keycloak.quarkus.runtime.KeycloakMain] (main) Running the server in development mode. DO NOT use this configuration in production.
```

Ezek után a <http://localhost:8081> címen állítsuk be a realm-et ízlés szerint,
nem megfeledkezve a felhasználókról és a jelszavaikról sem. Majd a konténerben:

```
<Ctrl-C>
2024-08-14 08:39:39,390 INFO  [io.quarkus] (Shutdown thread) Keycloak stopped in 0.030s

bash-5.1$ /opt/keycloak/bin/kc.sh export --realm ets --file /tmp/ets-realm.json
...
2024-08-14 08:40:02,293 INFO  [org.keycloak.services] (main) KC-SERVICES0034: Export of realm 'ets' requested.
2024-08-14 08:40:02,293 INFO  [org.keycloak.exportimport.singlefile.SingleFileExportProvider] (main) Exporting realm 'ets' into file /tmp/ets-realm.json
2024-08-14 08:40:03,026 INFO  [org.keycloak.services] (main) KC-SERVICES0035: Export finished successfully
...
2024-08-14 08:40:03,371 INFO  [io.quarkus] (main) Keycloak stopped in 0.246s
```

Végezetül egy másik terminálban (miközben még fut a konténer!):

```
$ docker compose cp keycloak-export:/tmp/ets-realm.json .
Successfully copied 76.3kB to .
```

Ezután kiléphetünk a futó konténerből.

<!-- Local Variables: -->
<!-- markdown-toc-header-toc-title: **Tartalom** -->
<!-- End: -->
