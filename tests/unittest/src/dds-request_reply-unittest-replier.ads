-- ---------------------------------------------------------------------
with DDS.String_DataReader;
with DDS.String_DataWriter;
with DDS.Request_Reply.Replier.Typed_Replier_Generic;
package DDS.Request_Reply.Unittest.Replier is new DDS.Request_Reply.Replier.Typed_Replier_Generic
  (Request_DataReader => String_DataReader,
   Reply_DataWriter   => String_DataWriter);
