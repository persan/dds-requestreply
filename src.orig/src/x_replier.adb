--  /* $Id$
--
--   (c) Copyright, Real-Time Innovations, 2012-2016.
--   All rights reserved.
--   No duplications, whole or partial, manual or electronic, may be made
--   without express written permission.  Any such copies, or
--   revisions thereof, must display this notice unaltered.
--   This code contains trade secrets of Real-Time Innovations, Inc.
--
--   modification history
--  ---------------------
--  1.0a,2mar12,jch  Created.
--  ============================================================================ */
--
--  #include "log/log_makeheader.h"
--
--  #ifndef log_common_h
--    #include "log/log_common.h"
--  #endif
--
--  #ifndef connext_c_replier_h
--    #include "connext_c/connext_c_replier.h"
--  #endif
--
--  #include "connext_c/connext_c_simple_replier.h"
--
--  #ifndef connext_c_replier_impl_h
--      #include "connext_c/connext_c_replier_impl.h"
--  #endif
--
--  #include "dds_c/dds_c_log_impl.h"
--
--  #include "connext_c/connext_c_untyped_impl.h"
--
--  #include "UntypedCommon.pkg.h"
--
--  #include "connext_c/generic/connext_c_requestreply_TReqTRepReplier.gen"
--
--  #define DDS_CURRENT_SUBMODULE  DDS_SUBMODULE_MASK_DATA
--
package body replier is
   pragma warnings (off);
   -------------------------------------------
   -- RTI_Connext_Replier_on_data_available --
   -------------------------------------------

   procedure RTI_Connext_Replier_on_data_available
     (listener_data : Interfaces.C.Extensions.void_ptr;
      reader        : DDS.DataReader.Ref_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "RTI_Connext_Replier_on_data_available unimplemented");
      raise Program_Error
        with "Unimplemented procedure RTI_Connext_Replier_on_data_available";
   end RTI_Connext_Replier_on_data_available;

   ----------------------------------------------
   -- RTI_Connext_ReplierParams_toEntityParams --
   ----------------------------------------------

   procedure RTI_Connext_ReplierParams_toEntityParams
     (self     : in     RTI_Connext_ReplierParams;
      toParams :    out RTI_Connext_EntityParams)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "RTI_Connext_ReplierParams_toEntityParams unimplemented");
      raise Program_Error
        with "Unimplemented procedure RTI_Connext_ReplierParams_toEntityParams";
   end RTI_Connext_ReplierParams_toEntityParams;

   -------------------------------------------
   -- RTI_Connext_Replier_wait_for_requests --
   -------------------------------------------



--
--  void RTI_Connext_Replier_on_data_available(
--      void* listener_data, DDS_DataReader* reader)
--  {
--      DDSLog_preconditionOnly(
--          const char * METHOD_NAME = "RTI_Connext_Replier_on_data_available";)
--      RTI_Connext_Replier* self = (RTI_Connext_Replier*) listener_data;
--
--      DDSLog_testPrecondition(self != NULL, return)
--      DDSLog_testPrecondition(self->listener.on_request_available != NULL, return)
--
--      self->listener.on_request_available(&self->listener, self);
--  }


--  DDS_ReturnCode_t RTI_Connext_ReplierParams_toEntityParams(
--      const RTI_Connext_ReplierParams* self, RTI_Connext_EntityParams* toParams)
--  {
--      DDSLog_preconditionOnly(
--          const char* METHOD_NAME = "RTI_Connext_ReplierParams_toEntityParams";)
--
--      DDSLog_testPrecondition(self == NULL, return DDS_RETCODE_PRECONDITION_NOT_MET);
--      DDSLog_testPrecondition(toParams == NULL, return DDS_RETCODE_PRECONDITION_NOT_MET);
--      toParams->participant = self->participant;
--      toParams->datareader_qos = self->datareader_qos;
--      toParams->datawriter_qos = self->datawriter_qos;
--      toParams->publisher = self->publisher;
--      toParams->qos_library_name = self->qos_library_name;
--      toParams->qos_profile_name = self->qos_profile_name;
--      toParams->reply_topic_name = self->reply_topic_name;
--      toParams->request_topic_name = self->request_topic_name;
--      toParams->service_name = self->service_name;
--      toParams->subscriber = self->subscriber;
--
--      return DDS_RETCODE_OK;
--  }
--
--  DDS_ReturnCode_t RTI_Connext_Replier_delete(RTI_Connext_Replier * self)
--  {
--      DDS_ReturnCode_t retcode = DDS_RETCODE_OK;
--      const char* METHOD_NAME = "RTI_Connext_Replier_delete";
--
--      if(self == NULL) {
--          DDSLog_exception(METHOD_NAME, &DDS_LOG_BAD_PARAMETER_s,
--                           "self");
--          return DDS_RETCODE_BAD_PARAMETER;
--      }
--
--      if (self->_impl != NULL) {
--          retcode = RTI_Connext_EntityUntypedImpl_delete(self->_impl);
--          if(retcode != DDS_RETCODE_OK) {
--              DDSLog_exception(METHOD_NAME, &RTI_LOG_ANY_FAILURE_s,
--                  "Failure deleting impl");
--          }
--      }
--
--      RTIOsapiHeap_free(self);
--
--      return retcode;
--  }

   procedure RTI_Connext_Replier_Wait_For_Requests (Self        : not null access RTI_Connext_Replier;
                                                    Min_Count   : DDS.Natural;
                                                    Max_Wait    : out DDS.Duration_T) is
   begin
      RTI_Connext_Replier'Class (Self.all)'Access.RTI_Connext_EntityUntypedImpl_Wait_For_Any_Sample (Min_Count, Max_Wait);
   end;

--
--  /* ----------------------------------------------------------------- */
end replier;
