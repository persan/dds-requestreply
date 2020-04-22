with DDS.Request_Reply.Requester.Typed_Requester_Generic;
with Dds.Builtin_String_DataReader;
with Dds.Builtin_String_DataWriter;
package DDS.Request_Reply.Tests.Simple.Requester is new
  DDS.Request_Reply.Requester.Typed_Requester_Generic
    (Request_DataWriter => Dds.Builtin_String_DataWriter,
     Reply_DataReader   => Dds.Builtin_String_DataReader);
