To update a container image:

* [Authenticate](https://docs.gitlab.com/ee/user/packages/container_registry/#authenticate-with-the-container-registry) with the GitLab Container Registry

* Build and push an image:

```
 $ image="base/ubuntu-18.04"
 $ dockerfile="base/Dockerfile.ubuntu-18.04"
 $ date=$(date +"%Y%m%d")
 $ docker build --pull -t registry.gitlab.com/lilypond/lilypond/$image:$date -f $dockerfile .
 $ docker push registry.gitlab.com/lilypond/lilypond/$image:$date
```

*Note*: If building an image based on one that you did not yet push, omit the `--pull` flag to `docker-build`.

Combinations of `$container` and `$dockerfile` are:

| `$image` | `$dockerfile` | Comment |
| --- | --- | --- |
| `base/ubuntu-18.04` | `base/Dockerfile.ubuntu-18.04` | The "base" image |
| `ci/ubuntu-18.04` | `ci/Dockerfile.ubuntu-18.04` | The image for CI testing |
| `doc/ubuntu-18.04` | `doc/Dockerfile.ubuntu-18.04` | The image for building the official documentation |
