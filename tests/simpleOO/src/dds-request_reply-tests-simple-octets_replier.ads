with DDS.Request_Reply.Replier.Typed_Replier_Generic;
with Dds.Builtin_Octets_DataReader;
with Dds.Builtin_Octets_DataWriter;
package DDS.Request_Reply.Tests.Simple.Octets_Replier is new
  DDS.Request_Reply.Replier.Typed_Replier_Generic
    (Reply_DataWriter     => Dds.Builtin_Octets_DataWriter,
     Request_DataReader   => Dds.Builtin_Octets_DataReader);
