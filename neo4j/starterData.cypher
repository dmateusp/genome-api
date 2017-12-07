CREATE (p:Person { name: 'Daniel', role: 'Software Engineer', slack: '@dpires', email: 'dpires@gilt.com' });
CREATE (t:Team { name: 'DataTeam', role: 'Data and stuff', slackChannel: '#data' });
CREATE (ms: MicroService { name: 'svc-event', github: 'gilt/svc-event', lastCommit: '2017-01-02', lastDeployment: '2017-01-02'});
MATCH (p:Person),(t:Team)
WHERE p.email = 'dpires@gilt.com' AND t.name = 'DataTeam'
CREATE (p)-[r:MEMBER_OF]->(t);
MATCH (t:Team),(ms:MicroService)
WHERE t.name = 'DataTeam' AND ms.name = 'svc-event'
CREATE (t)-[r:OWNS]->(ms);