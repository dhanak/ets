# Elektronikus Tanársegéd (ETS)

Ez a BME műszaki informatika szakon oktatott, Deklaratív Programozás című tárgy
hallgatói információs rendszere, az ETS.

## Előzetes követelmények

* `docker`, `docker compose`
  * Debian/Ubuntu: `docker-ce`, `docker-ce-cli`, `docker-compose-plugin`
    csomagok
  * a régebbi, v1-es `docker-compose` python csomag nem jó!
* SICStus Prolog

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

## Emailek fejlesztői környezetben

Fejlesztői környezetben a rendszer nem küld éles emaileket, hanem egy
[MailCatcher](https://mailcatcher.me/) nevű beágyazott SMTP szerveren gyűjti
össze azokat. A kiküldött emaileket egy webes felületen keresztül lehet
megtekinteni a docker host gépen, a 1080-as porton:

```
$ xdg-open http://<docker-host>:1080
