# Elektronikus Tanársegéd (ETS)

Ez a BME műszaki informatika szakon oktatott, Deklaratív Programozás című tárgy
hallgatói információs rendszere, az ETS.

## Előzetes követelmények

* docker, docker-compose
* SICStus Prolog
* ...?

## Telepítés

A telepítéshez és futtatáshoz `docker` szükséges. Lépések:

### Futtatási környezet beállítása

1. Másold le az `env.test` file-t `env.live` (vagy hasonló) néven!

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
