-- ---------------------------------------------------------------------
--                                                                    --
--               Copyright (c) per.sandberg@bahnhof.se                --
--                                                                    --
--  Permission is hereby granted, free of charge, to any person       --
--  obtaining a copy of this software and associated documentation    --
--  files (the "Software"), to deal in the Software without           --
--  restriction, including without limitation the rights to use,      --
--  copy, modify, merge, publish, distribute, sublicense, and/or sell --
--  copies of the Software, and to permit persons to whom the Software--
--  is furnished to do so, subject to the following conditions:       --
--                                                                    --
--  The above copyright notice and this permission notice             --
--  (including the next paragraph) shall be included in all copies or --
--  substantial portions of the Software.                             --
--                                                                    --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF--
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND             --
--  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT       --
--  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,      --
--  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,--
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER     --
--  DEALINGS IN THE SOFTWARE.                                         --
--                                                                    --
--  <spdx: MIT>
--                                                                    --
-- ---------------------------------------------------------------------


package body DDS.Request_Reply.Replier.Impl is


   -----------------------
   -- Wait_For_Requests --
   -----------------------

   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T)
   is
   begin
      Self.Wait_For_Any_Sample (Max_Wait => Max_Wait , Min_Sample_Count => Min_Count);
   end Wait_For_Requests;

   --------------------------------
   -- Configure_Params_For_Reply --
   --------------------------------

   procedure Configure_Params_For_Reply
     (Self                 : not null access Ref;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T)
   is
   begin
      if Related_Request_Info =  AUTO_SAMPLE_IDENTITY then
         raise BAD_PARAMETER with "AUTO_SAMPLE_IDENTITY not allowed";
      end if;
      Params.Related_Sample_Identity := Related_Request_Info;
   end Configure_Params_For_Reply;

   procedure  Send_Sample
     (Self                 : not null access Ref;
      Data                 : System.Address;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T;
      MetpImpl         : not null access DDS.MetpTypeSupport.ref'Class) is
   begin
      Self.Configure_Params_For_Reply (Params, Related_Request_Info);
      Self.Writer.Write_W_Params (Data, Params,MetpImpl);
   end;

end DDS.Request_Reply.Replier.Impl;
