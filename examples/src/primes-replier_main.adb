with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with DDS.DomainParticipant;
with DDS.DomainParticipantFactory;
with Primes.PrimeNumberReplier;
with Primes.PrimeNumberRequest_TypeSupport;

with RTIDDS.Config;
procedure Primes.Replier_Main is
   use all type DDS.DomainParticipant.Ref_Access;
   use type DDS.ReturnCode_T;
   use type Dds.long;
   --   */
   --  #include "Primes.h"
   --  #include "PrimesSupport.h"
   --  
   --  #include "connext_c/connext_c_replier.h"
   --  
   --  /*
   --   * Instantiate the replier structure and declare its functions
   --   *
   --   */
   --  RTI_CONNEXT_REPLIER_DECL(PrimeNumberRequest, PrimeNumberReply, PrimeNumberReplier)
   --  
   --  /*
   --   * Define TReq, the request type and TRep, the reply type and
   --   * Instantiate the implementation of the replier functions.
   --   *
   --   * Note: for simplicity we include the implementation here, but it could
   --   *       be compiled in its own .c file.
   --   */
   --  #define TReq PrimeNumberRequest
   --  #define TRep PrimeNumberReply
   --  #define TReplier PrimeNumberReplier
   --  
   --  #include "connext_c/generic/connext_c_requestreply_TReqTRepReplier.gen"
   procedure Replier_Shutdown
     (Participant : in out DDS.DomainParticipant.Ref_Access;
      Replier     : in out PrimeNumberReplier.Ref_Access;
      Request     : in out PrimeNumberRequest_Access) is
   begin
      --  static int replier_shutdown(
      --  {
      --      DDS_ReturnCode_t retcode;
      --      int status = 0;
      --  
      --      if (request != NULL) {
      --          PrimeNumberRequestTypeSupport_delete_data(request);
      --      }
      --  
      --      if (replier != NULL) {
      --          retcode = RTI_Connext_Replier_delete(
      --              (RTI_Connext_Replier *) replier);
      --          if (retcode != DDS_RETCODE_OK) {
      --              fprintf(stderr, "delete replier error %d\n", retcode);
      --              status = -1;
      --          }
      --      }
      --  
      --      if (participant != NULL) {
      --          retcode = DDS_DomainParticipant_delete_contained_entities(participant);
      --          if (retcode != DDS_RETCODE_OK) {
      --              fprintf(stderr, "delete_contained_entities error %d\n", retcode);
      --              status = -1;
      --          }
      --  
      --          retcode = DDS_DomainParticipantFactory_delete_participant(
      --              DDS_TheParticipantFactory, participant);
      --          if (retcode != DDS_RETCODE_OK) {
      --              fprintf(stderr, "delete_participant error %d\n", retcode);
      --              status = -1;
      --          }
      --      }
      --      
      --      retcode = DDS_DomainParticipantFactory_finalize_instance();
      --      if (retcode != DDS_RETCODE_OK) {
      --          printf("ParticipantFactory finalize_instance error %d\n", retcode);
      --          status = -1;
      --      }
      --  
      --      return status;
      --  }
      null;
   end;
   
   procedure Send_Error_Reply (Replier    : PrimeNumberReplier.Ref_Access;
                               Request    : PrimeNumberRequest;
                               Request_Id : DDS.SampleIdentity_T) is
      pragma Unreferenced (Request);
      Reply      : aliased PrimeNumberReply;
   begin
      Initialize (Reply);
      Reply.Status := REPLY_ERROR;
      Replier.Send_Reply (Reply, Request_Id);
      Finalize (Reply);
   end;
   
   
   procedure Calculate_And_Send_Primes (Replier    : PrimeNumberReplier.Ref_Access;
                                        Request    : PrimeNumberRequest;
                                        Request_Id : DDS.SampleIdentity_T) is
      pragma Unreferenced (Request_Id, Request, Replier);
   --  {
   --      DDS_ReturnCode_t retcode;
   --      int i, m, k, length;
   --      int n = request->n;
   --      int primes_per_reply = request->primes_per_reply;
   --      int * prime = NULL;
      Reply      : aliased PrimeNumberReply;
   begin
      Initialize (Reply);
      --      DDS_LongSeq_set_maximum(&reply->primes, primes_per_reply);
      --  
      --      reply->status = REPLY_IN_PROGRESS;
      --  
      --      /* prime[i] indicates if i is a prime number */
      --      prime = (int *) malloc((n+1) * sizeof(int));
      --      if (prime == 0) {
      --          fprintf(stderr, "Bad allocation\n");
      --          send_error_reply(replier, request, request_id);
      --          return -1;
      --      }
      --  
      --      for (i = 0; i < n; i++) {
      --          prime[i] = 1;
      --      }
      --  
      --      prime[0] = 0;
      --      prime[1] = 0;
      --  
      --      m = (int) sqrt(n);
      --  
      --      for (i = 2; i <= m; i++) {
      --          if (prime[i]) {
      --              for (k = i*i; k <= n; k+=i) {
      --                  prime[k] = 0;
      --              }
      --  
      --              /* Add a new element */
      --              length = DDS_LongSeq_get_length(&reply->primes);
      --              DDS_LongSeq_set_length(&reply->primes,  length + 1);
      --              *DDS_LongSeq_get_reference(&reply->primes, length) = i;
      --  
      --              if (length + 1 == primes_per_reply) {
      --  
      --                  /* Send a reply now */
      --                  retcode = PrimeNumberReplier_send_reply(
      --                      replier, reply, request_id);
      --  
      --                  if (retcode != DDS_RETCODE_OK) {
      --                      fprintf(stderr, "send_reply error %d\n", retcode);
      --                      PrimeNumberReplyTypeSupport_delete_data(reply);
      --                      return -1;
      --                  }
      --  
      --                  DDS_LongSeq_set_length(&reply->primes, 0);
      --              }
      --          }
      --      }
      --  
      --      /* Calculation is done. Send remaining prime numbers */
      --      for (i = m + 1; i <= n; i++) {
      --          if (prime[i]) {
      --  
      --              length = DDS_LongSeq_get_length(&reply->primes);
      --              DDS_LongSeq_set_length(&reply->primes,  length + 1);
      --              *DDS_LongSeq_get_reference(&reply->primes, length) = i;
      --  
      --              if (length + 1 == primes_per_reply) {
      --  
      --                  /* Send a reply now */
      --                  retcode = PrimeNumberReplier_send_reply(
      --                      replier, reply, request_id);
      --  
      --                  if (retcode != DDS_RETCODE_OK) {
      --                      fprintf(stderr, "send_reply error %d\n", retcode);
      --                      PrimeNumberReplyTypeSupport_delete_data(reply);
      --                      return -1;
      --                  }
      --  
      --                  DDS_LongSeq_set_length(&reply->primes, 0);
      --              }
      --          }
      --      }
      --  
      --      /* Send the last reply. Indicate that the calculation is complete and
      --       * send any prime number left in the sequence
      --       */
      --      reply->status = REPLY_COMPLETED;
      --      retcode = PrimeNumberReplier_send_reply(
      --          replier, reply, request_id);
      --  
      --      if (retcode != DDS_RETCODE_OK) {
      --          fprintf(stderr, "send_reply error %d\n", retcode);
      --          PrimeNumberReplyTypeSupport_delete_data(reply);
      --          return -1;
      --      }
      --  
      --      free(prime);
      --      PrimeNumberReplyTypeSupport_delete_data(reply);
      --      return 0;
   end;
   
   
   procedure Replier_Main (Domain_Id : DDS.DomainId_T) is
      Factory          : constant DDS.DomainParticipantFactory.Ref_Access := DDS.DomainParticipantFactory.Get_Instance;
      Participant      : DDS.DomainParticipant.Ref_Access;
      Replier          : PrimeNumberReplier.Ref_Access;
      Request_Id       : Dds.SampleIdentity_T;
      Request_Info     : aliased DDS.SampleInfo;
      Request          : PrimeNumberRequest_Access := PrimeNumberRequest_TypeSupport.Create_Data;      
      MAX_WAIT         : constant DDS.Duration_T := DDS.To_Duration_T (20.0);
      RetCode          : DDS.ReturnCode_T := DDS.RETCODE_OK;
     
      

   begin
      --      /* Create the participant */
      Participant := Factory.Create_Participant (Domain_Id);
      if Participant = null then
         Put_Line (Standard_Error, "create_participant error");
         return;
      end if;

      --   Create the replier with that participant, and a QoS profile
      --       * defined in USER_QOS_PROFILES.xml
      Replier := PrimeNumberReplier.Create (Participant, Service_Name , Qos_Library_Name, Qos_Profile_Name);
      Request := PrimeNumberRequest_TypeSupport.Create_Data;
      
      --      /*
      --       * Receive requests and process them
      --       */
      Retcode := Replier.Receive_Request (Request.all, Request_Info, Timeout => MAX_WAIT);
      
      while Retcode = DDS.RETCODE_OK  loop
         if Request_Info.Valid_Data then
            DDS.Get_Sample_Identity (Request_Info, Request_Id);
            
            --  This constant is defined in Primes.idl */
            if Request.N <= 0 or
              Request.Primes_Per_Reply <= 0 or
              Request.Primes_Per_Reply > PRIME_SEQUENCE_MAX_LENGTH then         
               Put_Line (Standard_Error, "Cannot process request");
               Send_Error_Reply (Replier, Request.all, Request_Id);
            else
         
               Put_Line ("Calculating prime numbers below " & Request.N'Img & "... ");    
               --   This operation could be executed in a separate thread,
               --   to process requests in parallel
               Calculate_And_Send_Primes (Replier, Request.all, Request_Id);
               
               Put_Line ("DONE");
            end if;
         end if;
         Retcode := Replier.Receive_Request (Request.all, Request_Info, Timeout => MAX_WAIT);
      end loop;

      if Retcode = DDS.RETCODE_TIMEOUT then
         Put_Line ("No request received for " & MAX_WAIT.Sec'Img & " seconds. Shutting down replier");
      else 
         Put_Line (Standard_Error, "Error in replier " &  Retcode'Img);
      end if;
      
      Replier_Shutdown (Participant, Replier, Request);

   end;
   
   Domain_Id        : DDS.DomainId_T :=  0;
begin
   
   if Ada.Command_Line.Argument_Count > 0 then
      Domain_Id := DDS.DomainId_T'Value (Ada.Command_Line.Argument (1));
   end if;

   RTIDDS.Config.Logger.Get_Instance.Set_Verbosity (RTIDDS.Config.VERBOSITY_SILENT);
   -- Uncomment this to turn on additional logging
   -- RTIDDS.Config.Logger.Get_Instance.Set_Verbosity (RTIDDS.Config.VERBOSITY_WARNING);

   Put_Line ("PrimeNumberRequester: Sending a request to calculate the ");
   Put_Line ("(on domain %d)" & Domain_Id'Img);

   Replier_Main (Domain_Id);

end Primes.Replier_Main;
