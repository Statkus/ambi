package Mutex is

   protected type T_Mutex is

      ----------------------------------------------------------------------------------------------
      -- Lock
      ----------------------------------------------------------------------------------------------
      entry Lock;

      ----------------------------------------------------------------------------------------------
      -- Unlock
      ----------------------------------------------------------------------------------------------
      procedure Unlock;

   private

      Locked : Boolean := False;

   end T_Mutex;

end Mutex;