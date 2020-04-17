with DDS.Request_Reply.Typed_Requester_Generic;
with Dds.Builtin_String_DataReader;
with Dds.Builtin_String_DataWriter;
package DDS.Request_Reply.Tests.Simple.Requester is new
  DDS.Request_Reply.Typed_Requester_Generic (Request_DataWriters => Dds.Builtin_String_DataWriter,
                                             Reply_DataReaders   => Dds.Builtin_String_DataReader);
