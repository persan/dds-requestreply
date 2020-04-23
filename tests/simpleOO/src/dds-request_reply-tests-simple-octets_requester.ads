with DDS.Request_Reply.Requester.Typed_Requester_Generic;
with Dds.Builtin_Octets_DataReader;
with Dds.Builtin_Octets_DataWriter;
package DDS.Request_Reply.Tests.Simple.Octets_Requester is new
  DDS.Request_Reply.Requester.Typed_Requester_Generic
    (Request_DataWriter => Dds.Builtin_Octets_DataWriter,
     Reply_DataReader   => Dds.Builtin_Octets_DataReader);
