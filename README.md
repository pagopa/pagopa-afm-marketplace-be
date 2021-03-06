# pagopa-afm-marketplace-be
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=pagopa_pagopa-afm-marketplace-be&metric=alert_status)](https://sonarcloud.io/project/overview?id=pagopa_pagopa-afm-marketplace-be)
Spring Boot backend for Advanced Fee Management marketplace

- [pagopa-afm-marketplace-be](#pagopa-afm-marketplace-be)
    * [Api Documentation ๐](#api-documentation---)
    * [Technology Stack](#technology-stack)
    * [Start Project Locally ๐](#start-project-locally---)
        + [Prerequisites](#prerequisites)
        + [Run docker container](#run-docker-container)
    * [Develop Locally ๐ป](#develop-locally---)
        + [Prerequisites](#prerequisites-1)
        + [Run the project](#run-the-project)
            - [Spring Profiles](#spring-profiles)
        + [Testing ๐งช](#testing---)
            - [Unit testing](#unit-testing)
            - [Integration testing](#integration-testing)
            - [Load testing](#load-testing)
    * [Contributors ๐ฅ](#contributors---)
        + [Mainteiners](#mainteiners)


---
## Api Documentation ๐
See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-afm-marketplacebe-be/main/openapi/openapi.json)

---

## Technology Stack
- Java 11
- Spring Boot
- Spring WebFlux
- Spring Data Cosmos

---

## Start Project Locally ๐

### Prerequisites
- docker
- TODO

### Run docker container
`docker-compose up --build`

TODO

---

## Develop Locally ๐ป

### Prerequisites
- git
- maven
- jdk-11
- docker

### Run the project
TODO

#### Spring Profiles

- **local**: to develop locally using the docker DB.
- TODO

### Testing ๐งช

#### Unit testing

by `junit` typing `mvn clean verify`

#### Integration testing

by `newman` & `postman` collection 
- automatically  via Azure pipeline ( see `.devops` folder )
- TODO


#### Load testing

by `k6` & `postman` collection 
- automatically via Azure pipeline ( see `.devops` folder )
- TODO

## Contributors ๐ฅ
Made with โค๏ธ from PagoPa S.p.A.

### Mainteiners
See `CODEOWNERS` file
