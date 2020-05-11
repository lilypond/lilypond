To build and push fresh containers:
```
 $ docker build --pull -t registry.gitlab.com/lilypond/lilypond/ci/ubuntu-16.04:<date> -f Dockerfile.ubuntu-16.04 .
 $ docker push registry.gitlab.com/lilypond/lilypond/ci/ubuntu-16.04:<date>

 $ docker build --pull -t registry.gitlab.com/lilypond/lilypond/ci/ubuntu-18.04:<date> -f Dockerfile.ubuntu-18.04 .
 $ docker push registry.gitlab.com/lilypond/lilypond/ci/ubuntu-18.04:<date>
```
