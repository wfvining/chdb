Master
======

There is a single master server. The master spawns all slaves and directs
client messages to a slave capable of handling the client's request. The 
master is also responsible for directing replication of documents.

Slaves
======

The slave processes handle all the data, they are distributed on nodes
throughout the network. Clients communicate directly with the slaves,
after an initial exchange with the server.

Startup
=======

The master starts, and spawns all the slave nodes and processes. When
the slaves start they send a message to the master with a list of the
DocumentIds (and the versions of those Documents) that the slave
has. The server builds and maintains a mapping of DocumentIds (and
versions) to slave Pids (OR SendPorts!). The master accepts the first
response for a given DocumentId, until a newer version of that
document is discovered. If a newer version of a Document is
discovered, The new version is made the working copy and the old
version is the backup. The Document is queued for replication.
