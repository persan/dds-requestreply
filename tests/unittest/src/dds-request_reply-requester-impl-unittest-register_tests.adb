separate (DDS.Request_Reply.Requester.Impl.Unittest)
--  begin read only
procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
   Register_Routine (Test, Test_Setup'Access, "Test_Setup");
   Register_Routine (Test, Test_Create_Correlation_Cft'Access, "Test_Create_Correlation_Cft");
   Register_Routine (Test, Test_Create_Reader_Topic'Access, "Test_Create_Reader_Topic");
   Register_Routine (Test, Test_Create_Writer_Topic'Access, "Test_Create_Writer_Topic");
   Register_Routine (Test, Test_Create'Access, "Test_Create");
   Register_Routine (Test, Test_Create_Query_Expression_For_Correlation_Sequence_Number'Access, "Test_Create_Query_Expression_For_Correlation_Sequence_Number");
   Register_Routine (Test, Test_Create_Correlation_Condition'Access, "Test_Create_Correlation_Condition");
   Register_Routine (Test, Test_Wait_For_Replies_For_Related_Request'Access, "Test_Wait_For_Replies_For_Related_Request");
   Register_Routine (Test, Test_Wait_For_Replies'Access, "Test_Wait_For_Replies");
   Register_Routine (Test, Test_Wait_For_Replies2'Access, "Test_Wait_For_Replies2");
   Register_Routine (Test, Test_Get_Reply_Loaned'Access, "Test_Get_Reply_Loaned");
   Register_Routine (Test, Test_Tear_Down'Access, "Test_Tear_Down");
end Register_Tests;
--  end read only
