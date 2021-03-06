package body DDS.Request_Reply.requester is
 
   --  DDS_ReturnCode_t RTI_Connext_Requester_delete(RTI_Connext_Requester * self)
   --  {
   --      DDS_ReturnCode_t retCode = DDS_RETCODE_OK;
   --  
   --      if(self == NULL) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                           "self");
   --          return DDS_RETCODE_BAD_PARAMETER;
   --      }
   --  
   --      if(self->_impl != NULL) {
   --          retCode = RTI_Connext_EntityUntypedImpl_delete(self->_impl);
   --      }
   --  
   --      if(retCode != DDS_RETCODE_OK) {
   --          DDSLog_exception(&RTI_LOG_ANY_FAILURE_s,
   --                           "delete RequesterUntypedImpl");
   --      }
   --  
   --      RTIOsapiHeap_free(self);
   --      return retCode;
   --  }
   --  
   --  DDS_ReturnCode_t RTI_Connext_Requester_wait_for_replies_for_related_request(
   --      RTI_Connext_Requester* self,
   --      DDS_Long min_reply_count,
   --      const struct DDS_Duration_t* max_wait,
   --      const struct DDS_SampleIdentity_t* related_request_info)
   --  {
   --      DDS_ReturnCode_t retCode = DDS_RETCODE_OK;
   --  
   --  
   --      if (related_request_info != NULL) {
   --          retCode = RTI_Connext_RequesterUntypedImpl_wait_for_replies(
   --              self->_impl, max_wait, min_reply_count, related_request_info);
   --      } else {
   --          retCode = RTI_Connext_EntityUntypedImpl_wait_for_any_sample(
   --              self->_impl, max_wait, min_reply_count);
   --      }
   --  
   --      if(retCode != DDS_RETCODE_OK && retCode != DDS_RETCODE_TIMEOUT) {
   --          DDSLog_exception(&RTI_LOG_ANY_FAILURE_s,
   --                           "wait for samples");
   --      }
   --      return retCode;
   --  }
   --  
   --  DDS_ReturnCode_t RTI_Connext_Requester_wait_for_replies(
   --      RTI_Connext_Requester* self,
   --      DDS_Long min_reply_count,
   --      const struct DDS_Duration_t* max_wait)
   --  {
   --      return RTI_Connext_Requester_wait_for_replies_for_related_request(
   --          self, min_reply_count, max_wait, NULL);
   --  }
   --  
   --  /* ----------------------------------------------------------------- */
   --  /* End of $Id$ */
end DDS.Request_Reply.requester;
