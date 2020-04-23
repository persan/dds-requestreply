with DDS.Request_Reply.Replier.Typed_Replier_Generic;
with Dds.Builtin_String_DataReader;
with Dds.Builtin_String_DataWriter;
package DDS.Request_Reply.Tests.Simple.String_Replier is new
  DDS.Request_Reply.Replier.Typed_Replier_Generic
    (Reply_DataWriter     => Dds.Builtin_String_DataWriter,
     Request_DataReader   => Dds.Builtin_String_DataReader);
