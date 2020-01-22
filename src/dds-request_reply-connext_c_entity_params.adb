pragma Ada_2012;
package body DDS.Request_Reply.Connext_C_Entity_Params is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out RTI_Connext_EntityParams) is
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out RTI_Connext_EntityParams) is
   begin
      Dds.Finalize (Object.Service_Name);
      Dds.Finalize (Object.Request_Topic_Name);
      Dds.Finalize (Object.Reply_Topic_Name);
      Dds.Finalize (Object.Qos_Library_Name);
      Dds.Finalize (Object.Qos_Profile_Name);
   end Finalize;
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
   --  /*#include <stdlib.h>*/
   --
   --  #ifndef log_common_h
   --    #include "log/log_common.h"
   --  #endif
   --
   --  #ifndef connext_cpp_entity_params_h
   --    #include "connext_c/connext_c_entity_params.h"
   --  #endif
   --
   --  #include "dds_c/dds_c_log_impl.h"
   --
   --  #define DDS_CURRENT_SUBMODULE  DDS_SUBMODULE_MASK_DATA
   --
   --  void RTI_Connext_EntityParams_delete(RTI_Connext_EntityParams* self)
   --  {
   --      RTIOsapiHeap_freeStructure(self);
   --  }
   --
   --  RTIBool RTI_Connext_EntityParams_validate(const RTI_Connext_EntityParams* self)
   --  {
   --      RTIBool retVal = RTI_TRUE;
   --      if (self->participant == NULL) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --              "A participant is required");
   --          return RTI_FALSE;
   --      }
   --
   --      if (self->service_name == NULL) {
   --          if(self->request_topic_name == NULL || self->reply_topic_name == NULL)
   --          {
   --              DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                      "Either service name or request and reply topics are required.");
   --              return RTI_FALSE;
   --          }
   --      } else {
   --          if(self->request_topic_name != NULL || self->reply_topic_name != NULL)
   --          {
   --              DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                      "Service name and topics are mutually exclusive.");
   --              return RTI_FALSE;
   --          }
   --      }
   --
   --      if (self->publisher != NULL &&
   --          DDS_Publisher_get_participant(self->publisher) != self->participant) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                  "The publisher belongs to a different participant");
   --          return RTI_FALSE;
   --      }
   --
   --      if (self->subscriber != NULL &&
   --          DDS_Subscriber_get_participant(self->subscriber) != self->participant) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                  "The subscriber belongs to a different participant");
   --          return RTI_FALSE;
   --      }
   --
   --      if (self->qos_library_name != NULL && self->qos_profile_name == NULL) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --                  "qos_library_name is set but qos_profile_name is not");
   --          return RTI_FALSE;
   --      }
   --
   --      if (self->qos_library_name == NULL && self->qos_profile_name != NULL) {
   --          DDSLog_exception(&DDS_LOG_BAD_PARAMETER_s,
   --              "qos_profile_name is set but qos_library_name is not");
   --          return RTI_FALSE;
   --      }
   --
   --      return retVal;
   --  }
   --
   --  /* ----------------------------------------------------------------- */
   --  /* End of $Id$ */
end DDS.Request_Reply.Connext_C_Entity_Params;
