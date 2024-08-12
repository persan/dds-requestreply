with DDS.String_DataReader;
with DDS.String_DataWriter;
with DDS.Request_Reply.Requester.Typed_Requester_Generic;
package DDS.Request_Reply.Unittest.Requester is new DDS.Request_Reply.Requester.Typed_Requester_Generic
  (Request_DataWriter => String_DataWriter,
   Reply_DataReader   => String_DataReader);
