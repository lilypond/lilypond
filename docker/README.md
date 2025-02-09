To update a container image:

* [Authenticate](https://docs.gitlab.com/ee/user/packages/container_registry/authenticate_with_container_registry.html) with the GitLab Container Registry

* Build and push an image:

```
 $ image="base/ubuntu-22.04"
 $ dockerfile="base/Dockerfile.ubuntu-22.04"
 $ date=$(date +"%Y%m%d")
 $ docker build --pull -t registry.gitlab.com/lilypond/lilypond/$image:$date -f $dockerfile .
 $ docker push registry.gitlab.com/lilypond/lilypond/$image:$date
```

*Note*: If building an image based on one that you did not yet push, omit the `--pull` flag to `docker-build`.

Combinations of `$container` and `$dockerfile` are:

| `$image` | `$dockerfile` | Comment |
| --- | --- | --- |
| `base/ubuntu-22.04` | `base/Dockerfile.ubuntu-22.04` | The "base" image |
| `ci/ubuntu-22.04` | `ci/Dockerfile.ubuntu-22.04` | The image for CI testing |
| `doc/ubuntu-22.04` | `doc/Dockerfile.ubuntu-22.04` | The image for building the official documentation |
