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
pragma Extensions_Allowed (all);
generic
   type Base is abstract tagged limited private;
package DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic is

   package Listners is
      type Ref is limited interface;
      type Ref_Access is access all Ref'Class;
      procedure Compute_And_Reply
        (Self      : not null access Ref;
         Replier   : Typed_Replier_Generic.Ref_Access;
         Data      : Request_DataReader.Treats.Data_Type;
         Id        : DDS.SampleIdentity_T) is abstract;
   end Listners;

   type Ref (Listner : not null Listners.Ref_Access) is abstract new Base and Replyer_Listeners.Ref with null record;
   type Ref_Access is access all Ref'Class;
   procedure On_Request_Avalible (Self      : not null access Ref;
                                  Replier   : not null access Typed_Replier_Generic.Ref'Class);

end DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic;
