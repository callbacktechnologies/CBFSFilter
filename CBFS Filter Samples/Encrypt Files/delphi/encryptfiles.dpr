(*
 * CBFS Filter 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of CBFS Filter in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.callback.com/cbfsfilter
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)

program encryptfiles;

uses
  Forms,
  encryptfilesf in 'encryptfilesf.pas' {FormEncryptfiles};

begin
  Application.Initialize;

  Application.CreateForm(TFormEncryptfiles, FormEncryptfiles);
  Application.Run;
end.


         