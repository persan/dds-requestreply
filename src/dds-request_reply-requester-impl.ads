--  ----------------------------------------------------------------------------
--  Note this is an implementation package and is subject to change att any time.
--  ----------------------------------------------------------------------------

with DDS.ReadCondition;
with DDS.Request_Reply.Impl;
private package DDS.Request_Reply.Requester.Impl is
   type Ref is abstract limited new DDS.Request_Reply.Impl.Ref and DDS.Request_Reply.Requester.Ref with record
      null;
   end record;

   type Ref_Access is access all Ref'Class;

   function Get_Request_Data_Writer
     (Self : not null access Ref)
      return DDS.DataWriter.Ref_Access;

   function Get_Reply_Data_Reader
     (Self : not null access Ref)
      return DDS.DataReader.Ref_Access;

   function Touch_Samples
     (Self           : not null access Ref;
      Max_Count      : DDS.Integer;
      Read_Condition : DDS.ReadCondition.Ref_Access) return Integer;

   function Wait_For_Any_Sample
     (Self             : not null access Ref;
      Max_Wait         : DDS.Duration_T;
      Min_Sample_Count : DDS.Integer) return DDS.ReturnCode_T;



end DDS.Request_Reply.Requester.Impl;
