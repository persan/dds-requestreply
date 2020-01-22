--  /*
--   * Instantiate the requester structure and declare its functions
--   */
--  RTI_CONNEXT_REQUESTER_DECL(PrimeNumberRequest, PrimeNumberReply, PrimeNumberRequester)
--
--  /*
--   * Define TReq, the request type and TRep, the reply type and
--   * Instantiate the implementation of the requester functions.
--   *
--   * Note: for simplicity we include the implementation here, but it could
--   *       be compiled in its own .c file.
--   */
--  #define TReq PrimeNumberRequest
--  #define TRep PrimeNumberReply
--  #define TRequester PrimeNumberRequester
--
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with DDS.DomainParticipant;
with DDS.DomainParticipantFactory;
with Primes.PrimeNumberRequester;
with Primes.PrimeNumberRequest_TypeSupport;
with DDS.Request_Reply.Connext_C_Requester;
procedure Primes.Requester_Main is
   use DDS.DomainParticipant;
   use all type DDS.ReturnCode_T;

   procedure Requester_Shutdown ( Participant : DDS.DomainParticipant.Ref_Access;
                                  Requester   : PrimeNumberRequester.Ref_Access;
                                  Request     : PrimeNumberRequest_Access) is
   begin
      --  {
      --      DDS_ReturnCode_t retcode;
      --      int status = 0;
      --
      --      if (request != NULL) {
      --          PrimeNumberRequestTypeSupport_delete_data(request);
      --      }
      --
      --      if (requester != NULL) {
      --          retcode = RTI_Connext_Requester_delete(
      --              (RTI_Connext_Requester *) requester);
      --          if (retcode != DDS_RETCODE_OK) {
      --              fprintf(stderr, "delete requester error %d\n", retcode);
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
      --  }      null;
      null;
   end;


   procedure Requester_Main (N                : DDS.long ;
                             Primes_Per_Reply : DDS.long ;
                             Domain_Id        : DDS.DomainId_T)
     --  int requester_main(int n, int primes_per_reply, int domain_id)
   is
   --      int in_progress;
   --      int i, j;

      Retcode : DDS.ReturnCode_T;

      Replies  : aliased PrimeNumberRequest_Seq.Sequence;
      Info_Seq : aliased DDS.SampleInfo_Seq.Sequence;
      Participant : DDS.DomainParticipant.Ref_Access;
      Requester : PrimeNumberRequester.Ref_Access;
      Request : PrimeNumberRequest_Access;
      MAX_WAIT : DDS.Duration_T := DDS.To_Duration_T (20.0);
      Requester_Params : DDS.Request_Reply.Connext_C_Requester.RTI_Connext_RequesterParams;
      use PrimeNumberRequester;
   begin

      --  Create the participant
      Participant := DDS.DomainParticipantFactory.Get_Instance.Create_Participant (Domain_Id);
      if Participant = null then
         Put_Line (Standard_Error, "create_participant error");
         Ada.Command_Line.Set_Exit_Status (ADa.Command_Line.Failure);
         return;
      end if;


      --  Create the requester with that participant, and a QoS profile
      --  defined in USER_QOS_PROFILES.xml
      --
      Requester_Params.Participant := Participant;
      DDS.Copy (Requester_Params.Service_Name, Service_Name);
      DDS.Copy (Requester_Params.Qos_Library_Name, Qos_Library_Name);
      DDS.Copy (Requester_Params.Qos_Profile_Name, Qos_Profile_Name);
      Requester := PrimeNumberRequester.Create_W_Params (Requester_Params);
      if Requester = null then
         Put_Line (Standard_Error, "create requester error");
         Ada.Command_Line.Set_Exit_Status (ADa.Command_Line.Failure);
         Requester_Shutdown (Participant, Requester, Request);
         return;
      end if;


      Request := PrimeNumberRequest_TypeSupport.Create_Data;
      if Request = null then
         Put_Line (Standard_Error, "Create data error");
         ADa.Command_Line.Set_Exit_Status (ADa.Command_Line.Failure);
         Requester_Shutdown (Participant, Requester, Request);
         return;
      end if;


      Request.N := N;
      Request.Primes_Per_Reply := Primes_Per_Reply;
      Retcode := Requester.Send_Request (Request);


      --  Send the request
      Retcode := Requester.Send_Request (Request);

      if retcode /= DDS.RETCODE_OK then
         Put_Line (Standard_Error, "send_request error:" & Retcode'Img );
         Ada.Command_Line.Set_Exit_Status (ADa.Command_Line.Failure);
         Requester_Shutdown (Participant, Requester, Request);
         return;
      end if;



      --      /* Receive replies */
      --      retcode = PrimeNumberRequester_receive_replies(
      --          requester, &replies, &info_seq,
      --          1, /* Wait for at least one reply */
      --          DDS_LENGTH_UNLIMITED, &MAX_WAIT);
      --
      --      in_progress = 1;
      --      while (retcode == DDS_RETCODE_OK && in_progress) {
      --
      --          for (i = 0; i < PrimeNumberReplySeq_get_length(&replies); i++) {
      --              struct PrimeNumberReply * reply =
      --                  PrimeNumberReplySeq_get_reference(&replies, i);
      --              struct DDS_SampleInfo * info =
      --                  DDS_SampleInfoSeq_get_reference(&info_seq, i);
      --
      --              if (info->valid_data) {
      --
      --                  for (j = 0; j < DDS_LongSeq_get_length(&reply->primes); j++) {
      --                      DDS_Long prime_number =
      --                          *DDS_LongSeq_get_reference(&reply->primes, j);
      --
      --                      printf("%d ", prime_number);
      --                  }
      --
      --                  if (reply->status != REPLY_IN_PROGRESS) {
      --                      in_progress = 0;
      --                      if (reply->status == REPLY_ERROR) {
      --                          fprintf(stderr, "Error in Replier\n");
      --                      } else { /* reply->status == REPLY_COMPLETED */
      --                          printf("DONE");
      --                          fflush(stdout);
      --                      }
      --                  }
      --
      --                  printf("\n");
      --              }
      --          }
      --
      --          /*
      --           * Return the loan to the middleware
      --           */
      --          PrimeNumberRequester_return_loan(requester, &replies, &info_seq);
      --
      --          if (in_progress) {
      --              retcode = PrimeNumberRequester_receive_replies(
      --                  requester, &replies, &info_seq,
      --                  1, DDS_LENGTH_UNLIMITED, &MAX_WAIT);
      --          }
      --      }
      --
      --      if (retcode != DDS_RETCODE_OK) {
      --          if (retcode == DDS_RETCODE_TIMEOUT) {
      --              fprintf(stderr, "Timed out waiting for prime numbers\n");
      --              return -1;
      --          } else {
      --              fprintf(stderr, "Error receiving replies %d\n", retcode);
      --              return -1;
      --          }
      --      }
      --
      --      return requester_shutdown(participant, requester, request);
      null;
   end;
begin
   null;
   --      int domain_id = 0;
   --      int n;
   --      int primes_per_reply = 5;
   --
   --      if (argc < 2) {
   --          printf("PrimeNumberRequester:\n");
   --          printf("Sends a request to calculate the prime numbers <= n\n");
   --          printf("Parameters: <n> [<primes_per_reply> = 5] [<domain_id> = 0]\n");
   --          return -1;
   --      }
   --
   --      n = atoi(argv[1]);
   --
   --      if (argc > 2) {
   --          primes_per_reply = atoi(argv[2]);
   --      }
   --
   --      if (argc > 3) {
   --          domain_id = atoi(argv[3]);
   --      }
   --
   --      /* Uncomment this to turn on additional logging
   --      NDDS_Config_Logger_set_verbosity(
   --                      NDDS_Config_Logger_get_instance(),
   --                      NDDS_CONFIG_LOG_VERBOSITY_WARNING);
   --      */
   --
   --      printf("PrimeNumberRequester: Sending a request to calculate the ");
   --      printf("prime numbers <=  %d in sequences of %d or less elements ",
   --             n, primes_per_reply);
   --      printf("(on domain %d)\n", domain_id);
   --      fflush(stdout);
   --
   --      return requester_main(n, primes_per_reply, domain_id);
   --  }

end;
