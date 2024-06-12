
# Maven

Prepara os arquivos do projeto.
Lista de Archetypes: https://maven.apache.org/archetypes/index.html

```
cd system
../dependencies/apache-maven-3.9.7/bin/mvn archetype:generate \
    -DgroupId=microservice \
    -DartifactId=microservice \
    -DarchetypeArtifactId=maven-archetype-quickstart \
    -DarchetypeVersion=1.4 \
    -DinteractiveMode=false
```

Configura versão do projeto.

```
cd system/microservice
../../dependencies/apache-maven-3.9.7/bin/mvn versions:set -DnewVersion=1.0.0 -DprocessAllModules -DgenerateBackupPoms=false
../../dependencies/apache-maven-3.9.7/bin/mvn versions:commit
```

Preparação do Maven Wrapper.

```
../../dependencies/apache-maven-3.9.7/bin/mvn wrapper:wrapper -Dtype=source
```

Geração de pacote Java, e execução do mesmo.

```
./mvnw clean package
java -cp target/microservice-1.0.0.jar microservice.Microservice
```


