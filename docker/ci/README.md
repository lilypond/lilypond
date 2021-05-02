To update the CI containers:

* [Log in](https://docs.gitlab.com/ee/user/packages/container_registry/#authenticating-to-the-gitlab-container-registry) to Gitlab's container registry

* Build and push the packages:

```
 $ DATE=$(date +'%Y%m%d')
 $ docker build --pull -t registry.gitlab.com/lilypond/lilypond/ci/ubuntu-18.04:${DATE} -f Dockerfile.ubuntu-18.04 .
 $ docker push registry.gitlab.com/lilypond/lilypond/ci/ubuntu-18.04:${DATE}
```
