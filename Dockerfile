FROM maven:3.9-amazoncorretto-21@sha256:81de222c1f34ac467bf968e1800b73fb41e714427c0212ba004296ea972e808a AS buildtime

WORKDIR /build
COPY . .

RUN mvn clean package -DskipTests

FROM amazoncorretto@sha256:ca805c030d45db58e93b2276580ea141aa7d33497009ab4c6b706c587a97e9b1 AS runtime

VOLUME /tmp
WORKDIR /app

COPY --from=buildtime /build/target/*.jar /app/app.jar
# The agent is enabled at runtime via JAVA_TOOL_OPTIONS.
ADD https://github.com/microsoft/ApplicationInsights-Java/releases/download/3.4.15/applicationinsights-agent-3.4.15.jar /app/applicationinsights-agent.jar

RUN chown -R nobody:nobody /app

EXPOSE 8080

ENTRYPOINT [ "java","-jar","/app/app.jar" ]