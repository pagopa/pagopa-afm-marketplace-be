# pagopa-afm-marketplace-be
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=pagopa-afm-marketplacebe-be&metric=alert_status)](https://sonarcloud.io/dashboard?id=pagopa-afm-marketplacebe-be)
Spring Boot backend for Advanced Fee Management marketplace

- [pagopa-afm-marketplace-be](#pagopa-afm-marketplace-be)
    * [Api Documentation 📖](#api-documentation---)
    * [Technology Stack](#technology-stack)
    * [Start Project Locally 🚀](#start-project-locally---)
        + [Prerequisites](#prerequisites)
        + [Run docker container](#run-docker-container)
    * [Develop Locally 💻](#develop-locally---)
        + [Prerequisites](#prerequisites-1)
        + [Run the project](#run-the-project)
            - [Spring Profiles](#spring-profiles)
        + [Testing 🧪](#testing---)
            - [Unit testing](#unit-testing)
            - [Integration testing](#integration-testing)
            - [Load testing](#load-testing)
    * [Contributors 👥](#contributors---)
        + [Mainteiners](#mainteiners)


---
## Api Documentation 📖
See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-afm-marketplacebe-be/main/openapi/openapi.json)

---

## Technology Stack
- Java 11
- Spring Boot
- Spring WebFlux
- Spring Data Cosmos

---

## Start Project Locally 🚀

### Prerequisites
- docker
- TODO

### Run docker container
`docker-compose up --build`

TODO

---

## Develop Locally 💻

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

### Testing 🧪

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

## Contributors 👥
Made with ❤️ from PagoPa S.p.A.

### Mainteiners
See `CODEOWNERS` file
