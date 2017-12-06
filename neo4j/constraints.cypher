CREATE CONSTRAINT ON (person:Person) ASSERT person.email IS UNIQUE;
CREATE CONSTRAINT ON (team:Team) ASSERT team.name IS UNIQUE;
CREATE CONSTRAINT ON (microservice:MicroService) ASSERT microservice.name IS UNIQUE;