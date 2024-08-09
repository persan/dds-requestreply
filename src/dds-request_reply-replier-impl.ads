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
with DDS.MetpTypeSupport;
with DDS.Request_Reply.Impl;
private package  DDS.Request_Reply.Replier.Impl is

   type Ref is limited new DDS.Request_Reply.Impl.Ref and DDS.Request_Reply.Replier.Ref with record
      null;
   end record;
   type Ref_Access is access all Ref;


   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T);

   procedure Configure_Params_For_Reply
     (Self                 : not null access Ref;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T);

   procedure  Send_Sample
     (Self                 : not null access Ref;
      data                 : System.Address;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T;
      MetpImpl         : not null access DDS.MetpTypeSupport.ref'Class);

end DDS.Request_Reply.Replier.Impl;
