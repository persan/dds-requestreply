with DDS.Request_Reply.Typed_Replier_Generic;
with Dds.Builtin_String_DataReader;
with Dds.Builtin_String_DataWriter;
package DDS.Request_Reply.Tests.Simple.Replier is new
  DDS.Request_Reply.Typed_Replier_Generic
    (Reply_DataWriters     => Dds.Builtin_String_DataWriter,
     Request_DataReaders   => Dds.Builtin_String_DataReader);