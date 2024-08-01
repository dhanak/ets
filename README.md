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

### Docker build

```sh
$ docker compose build
...

$ docker compose up -d
```

The site should be up and running, and accesible on the docker host at the
specified port (`HTTP_EXTERNAL_PORT`, defaults to 8080).
