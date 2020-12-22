//
// Copyright 2020 Electronic Arts Inc.
//
// The Command & Conquer Map Editor and corresponding source code is free
// software: you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.

// The Command & Conquer Map Editor and corresponding source code is distributed
// in the hope that it will be useful, but with permitted additional restrictions
// under Section 7 of the GPL. See the GNU General Public License in LICENSE.TXT
// distributed with this program. You should have received a copy of the
// GNU General Public License along with permitted additional restrictions
// with this program. If not, see https://github.com/electronicarts/CnC_Remastered_Collection

/// <summary>
/// This class contains encoders and decoders for the Westwood XOR Delta and LCW compression schemes.
/// </summary>


        ////////////////////////////////////////////////////////////////////////////////
        //  Notes
        ////////////////////////////////////////////////////////////////////////////////
        //
        // LCW streams should always start and end with the fill command (& 0x80) though
        // the decompressor doesn't strictly require that it start with one the ability
        // to use the offset commands in place of the RLE command early in the stream
        // relies on it. Streams larger than 64k that need the relative versions of the
        // 3 and 5 byte commands should start with a null byte before the first 0x80
        // command to flag that they are relative compressed.
        //
        // LCW uses the following rules to decide which command to use:
        // 1. Runs of the same colour should only use 4 byte RLE command if longer than
        //    64 bytes. 2 and 3 byte offset commands are more efficient otherwise.
        // 2. Runs of less than 3 should just be stored as is with the one byte fill
        //    command.
        // 3. Runs greater than 10 or if the relative offset is greater than
        //    4095 use an absolute copy. Less than 64 bytes uses 3 byte command, else it
        //    uses the 5 byte command.
        // 4. If Absolute rule isn't met then copy from a relative offset with 2 byte
        //    command.
        //
        // Absolute LCW can efficiently compress data that is 64k in size, much greater
        // and relative offsets for the 3 and 5 byte commands are needed.
        //
        // The XOR delta generator code works to the following assumptions
        //
        // 1. Any skip command is preferable if source and base are same
        // 2. Fill is preferable to XOR if 4 or larger, XOR takes same data plus at
        //    least 1 byte
        //
        ////////////////////////////////////////////////////////////////////////////////


// Translated to Lazarus Pascal  by DDF3 (steam)
// all arrays are static ,no pointers needed
// original C# version https://github.com/electronicarts/CnC_Remastered_Collection/blob/master/CnCTDRAMapEditor/Utility/WWCompression.cs
// used in C&C1 SHP,CPS,VQA  , RA MPR MapPack, OverLay ,SHP  etc files


unit LCW;



{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,math;

Const
XOR_SMALL: Byte = $7F;
XOR_MED  : Byte = $FF;
XOR_LARGE: Integer = $3FFF;
XOR_MAX  : Integer = $7FFF;


{auxillary function}
Function  LCWWorstCase(datasize : Integer) : Integer;
{auxillary function}
Function  XORWorstCase(datasize : Integer) : Integer;
{LCWDecompress (Format 80)}
function LcwDecompress(input :array of byte; ReadOffset: Integer;var output : array of byte; ReadEnd: Integer; OutEnd : Integer): Integer;
{GenerateXORDelta (Format 40)}
function GenerateXorDelta(source: array of Byte; ReadEnd: Integer; base : array of byte; BaseEnd: Integer;  var Dest : array of byte ): integer;
{ApplyXorDelta (Format 20)}
procedure ApplyXorDelta(xorSource: Array of Byte; var data : array of Byte ; xorStart :Integer; xorEnd : Integer; DataEnd : Integer);
{LCWCompress (Format 80)}
function LcwCompress(input : array of byte; var OutPut: array of Byte):Integer;




implementation

{auxillary function}
function LCWWorstCase(datasize: Integer): Integer;
begin
       result := datasize + (datasize div 63) +1;
end;
{auxillary function}
function XORWorstCase(datasize: Integer): Integer;
begin
       result := datasize +((datasize div 63) * 3) + 4;
end;

/// <summary>
      ///     Decompresses data in the proprietary LCW format used in many games
      ///     developed by Westwood Studios.
      /// </summary>
      /// <param name="input">The data to decompress.</param>
      /// <param name="readOffset">Location to start at in the input array.</param>
      /// <param name="output">The buffer to store the decompressed data. This is assumed to be initialized to the correct size.</param>
      /// <param name="readEnd">End offset for reading. Use 0 to take the end of the given data array.</param>
      /// <param name ="OutEnd  the size of the OutPut  or image width * height
      /// <returns>Length of the decompressed data in bytes.</returns>
     // public static Int32 LcwDecompress(Byte[] input, ref Int32 readOffset, Byte[] output, Int32 readEnd)
function LcwDecompress(input: array of byte; ReadOffset: Integer; var output: array of byte; ReadEnd: Integer; OutEnd : Integer): Integer;
var
relative                 : Boolean;
WriteEnd ,writeOffset ,s : Integer;
flag                     : byte;
cpysize ,offset          : word;
begin

          if  (0 = High(input)) or ( 0= High(output))then  result:= 0;
          relative := false;
          writeOffset := 0;
          WriteEnd := OutEnd;

          // Output length should be part of the information given in the file format using LCW.
          // Techncically it can just be cropped at the end, though this value is used to
          // automatically cut off repeat-commands that go too far.

          if readEnd <= 0 then  readEnd := ReadEnd;

          //Decide if the stream uses relative 3 and 5 byte commands
          //Extension allows effective compression of data > 64k
          //https://github.com/madmoose/scummvm/blob/bladerunner/engines/bladerunner/decompress_lcw.cpp
          // this is only used by later games for decoding hi-color vqa files.
          // For other stuff (like shp), just check in advance to decide if the data is too big
           if (readOffset >= readEnd) then  result:= writeOffset;
            if (input[readOffset] =0) then
            begin
                  relative := true;
                  inc(readOffset);
            end;
           while (writeOffset < writeEnd) do
           begin
               if (readOffset >= readEnd) then result:= writeOffset;
      	        flag := input[readOffset];
               inc(readOffset);
               if ((flag and $80) <> 0) then
                   begin
      	         if ((flag and $40) <> 0) then
                       begin
                            cpysize := (word((flag and $3F) + 3));
      		      //long set 0b11111110
      		      if (flag = $FE) then
                            begin
                                 if (readOffset >= readEnd) then result:= writeOffset;
                                 cpysize := input[readOffset];
                                 inc(ReadOffSet);
                                if (readOffset >= readEnd) then result := writeOffset;
                                cpysize := cpysize + (word((input[readOffset]) shl 8));
                                inc(readOffset);
                                if (cpysize > writeEnd - writeOffset) then cpysize := (word(writeEnd - writeOffset));
                                if (readOffset >= readEnd) then result := writeOffset;
                                //DEBUG_SAY("0b11111110 Source Pos %ld, Dest Pos %ld, Count %d\n", source - sstart - 3, dest - start, cpysize);
                                while cpysize>0 do
                                begin
                                      if (writeOffset >= writeEnd) then result := WriteOffset;
                                      output[writeoffset] := input[readOffset];
                                      dec(cpysize);
                                      Inc(writeoffset);
                                end;
                                   inc(readOffset);
                            end // flag = $FE
                            else
                            begin
                                 //long move, abs 0b11111111
      			   if (flag = $FF) then
                                 begin
      				if (readOffset >= readEnd) then result := writeOffset;
                                      cpysize := input[readOffset];
                                      inc(readoffset);
                                     if (readOffset >= readEnd) then result := writeOffset;
      			             cpysize :=cpysize+ (word((input[readOffset]) shl 8));
                                     inc(readOffset);
      			             if (cpysize > writeEnd - writeOffset) then  cpysize := (word(writeEnd - writeOffset));
                                     if (readOffset >= readEnd) then result := writeOffset;
                                    offset := input[readOffset];
                                    inc(readOffset);
                                    if (readOffset >= readEnd) then result:= writeOffset;
                                    offset := offset+ (word((input[readOffset]) shl 8));
                                    inc(readOffset);
                                    //extended format for VQA32
      			      if (relative =True) then s := writeOffset - offset else s := offset;
      			      //DEBUG_SAY("0b11111111 Source Pos %ld, Dest Pos %ld, Count %d, Offset %d\n", source - sstart - 5, dest - start, cpysize, offset);
                                    while (cpysize > 0) do
                                    begin
                                         if (writeOffset >= writeEnd) then result := writeoffset;
                                         output[writeOffset] := output[s];
                                         inc(writeOffset);
                                         Inc(s);
                                         dec(cpysize);
                                    end;
                                   //short move abs 0b11??????
                               end    //flag= $FF
                             	 else
      			    begin
      				if (cpysize > writeEnd - writeOffset) then cpysize := (word(writeEnd - writeOffset));
                                      if (readOffset >= readEnd) then result:= writeOffset;
                                      offset := input[readOffset];
                                      inc(readOffset);
                                      if (readOffset >= readEnd) then result := writeOffset;
                                      offset := offset+ (word((input[readOffset]) shl 8));
                                      inc(readOffset);
      				//extended format for VQA32
      				if (relative) then  s:= writeOffset - offset   else   s := offset;
      				//DEBUG_SAY("0b11?????? Source Pos %ld, Dest Pos %ld, Count %d, Offset %d\n", source - sstart - 3, dest - start, cpysize, offset);
                                      while cpysize >0 do
                                      begin
                                            if (writeOffset >= writeEnd) then result := writeOffset;
                                            output[writeOffset] := output[s];
                                            Inc(s);
                                            Inc(writeOffset);
                                            dec(cpysize);
                                      end;
                                  end;
      			end;
      		        //short copy 0b10??????
      		        end
                                else
      		               begin
      			        if (flag = $80)then
      			           begin
      				    //DEBUG_SAY("0b10?????? Source Pos %ld, Dest Pos %ld, Count %d\n", source - sstart - 1, dest - start, 0);
      				    Result:= writeOffset;
      			           end;
      			        cpysize := (word(flag and $3F));
      			        if (cpysize > writeEnd - writeOffset)then cpysize := (word(writeEnd - writeOffset));
      			        //DEBUG_SAY("0b10?????? Source Pos %ld, Dest Pos %ld, Count %d\n", source - sstart - 1, dest - start, cpysize);
                                      while cpysize >0 do
                                      begin
                                            if (readOffset >= readEnd ) or ( writeOffset >= writeEnd) then result:=writeOffset;
                                            output[writeOffset] := input[readOffset];
                                            Inc(writeOffset);
                                            Inc(readOffset);
                                            dec(cpysize);
                                      end;
                                  end;
      	                     //short move rel 0b0???????
                              end
      	                else
      	                   begin
      		              cpysize := (word((flag shr 4) + 3));
      		              if (cpysize > writeEnd - writeOffset)then  cpysize := (word(writeEnd - writeOffset));
                              if (readOffset >= readEnd) then result:= writeOffset;
      		              offset := (word(((flag and $F) shl 8) + input[readOffset]));
                              Inc(readOffset);
      		             //DEBUG_SAY("0b0??????? Source Pos %ld, Dest Pos %ld, Count %d, Offset %d\n", source - sstart - 2, dest - start, cpysize, offset);
                                   while cpysize >0 do
                                   begin
                                         if (writeOffset >= writeEnd ) Or( writeOffset < offset) then result := writeOffset;
                                         output[writeOffset] := output[writeOffset - offset];
                                         Inc(writeOffset);
                                       dec(cpysize);
                                   end;
                               end;
              end;
          // If buffer is full, make sure to skip end command!
          if (writeOffset = writeEnd) and (readOffset < readend) and (input[readOffset] = $80)then Inc(readOffset);
             result:= writeOffset;
end;


/// <summary>
/// Generates a binary delta between two buffers. Mainly used for image data.
/// </summary>
/// <param name="source">Buffer containing data to generate the delta for.</param>
/// <param name="base">Buffer containing data that is the base for the delta.</param>
/// <param name = dest buffer  containing the destination xored data </param>
/// <returns>The generated delta as bytes array.</returns>
/// <remarks>Commonly known in the community as "format40".</remarks>
function GenerateXorDelta(source: array of Byte; ReadEnd: Integer; base: array of byte; BaseEnd: Integer; var Dest: array of byte): integer;
var
putp       : Integer = 0;
getsp      : Integer = 0;
getbp      : Integer = 0;
getsendp   : integer;
fillcount  : Integer = 0;
xorcount   : Integer = 0;
skipcount  : Integer = 0;
lastxor    : Byte;
testsp     : Integer;
testbp     : Integer;
count      : word;
begin
         getsendp := Min(ReadEnd,BaseEnd);
         while (getsp < getsendp) do
          begin
               lastxor := byte(Source[getsp] XOR base[getbp]);
               testsp := getsp;
               testbp := getbp;
               //Only evaluate other options if we don't have a matched pair
              while (testsp < getsendp ) and ( source[testsp] <> base[testbp]) do
              begin
                  if ((source[testsp] XOR  base[testbp]) = lastxor) then
                  begin
                      Inc(fillcount);
                      Inc(xorcount);
                  end
                  else
                  begin
                      if (fillcount > 3) then  break;
                      lastxor := Byte(source[testsp] XOR base[testbp]);
                      fillcount := 1;
                      Inc(xorcount);
                  end;
                  Inc(testsp);
                  Inc(testbp);
              end;

         //fillcount should always be lower than xorcount and should be greater
         //than 3 to warrant using the fill commands.
         if fillcount > 3 then fillcount := fillcount else fillcount := 0;
         //Okay, lets see if we have any xor bytes we need to handle
         xorcount := xorcount - fillcount;
              while (xorcount <> 0) do
              begin
                   //It's cheaper to do the small cmd twice than do the large cmd once
                  //for data that can be handled by two small cmds.
                  //cmd 0???????
                  if (xorcount < XOR_MED) then
                  begin
                       if xorcount <= XOR_SMALL then count := word(xorcount) else count := word(XOR_SMALL);
                        dest[putp] := Byte(count);
                        Inc(PutP);
                      //cmd 10000000 10?????? ??????
                  end
                  else
                  begin
                       if xorcount <= XOR_LARGE then count := word(xorcount) else count := word(XOR_LARGE);
                       dest[putp] := $80;
                       Inc(putp);
                       dest[putp] := Byte((count and $FF));
                       Inc(putp);
                       dest[putp] := Byte((((count shr 8) and $FF) or $80));
                       Inc(putp);
                  end;
                 while (count <> 0) do
                  begin
                      dest[putp] := Byte((source[getsp] XOR base[getbp]));
                      inc(putp); Inc(getbp); inc(getsp);
                      dec(count);
                      dec(xorcount);
                  end;
              end;

              //lets handle the bytes that are best done as xorfill
              while (fillcount <> 0) do
              begin
                   //cmd 00000000 ????????
                  if (fillcount <= XOR_MED) then
                  begin
                      count := word(fillcount);
                      dest[putp] := 0;
                      inc(putp);
                      dest[putp] := Byte((count and $FF));
                      inc(putp);
                      //cmd 10000000 11?????? ??????
                  end
                  else
                      begin
                      if fillcount <= XOR_LARGE then count := word(fillcount) else count := word(XOR_LARGE);
                      dest[putp] := $80;
                      inc(putp);
                      dest[putp] := Byte((count and $FF));
                      inc(putp);
                      dest[putp] := Byte((((count shr 8) and $FF) Or $C0));
                      inc(putp);
                  end;
                  dest[putp] := Byte((source[getsp] XOR base[getbp]));
                  inc(putp);
                  fillcount := fillcount- count;
                  getsp := getsp + count;
                  getbp := getbp + count;
              end;
                 //Handle regions that match exactly
               while (testsp < getsendp) and  (source[testsp]= base[testbp])do
                 begin
                  inc(skipcount);
                  inc(testsp);
                  inc(testbp);
                end;

              while (skipcount <> 0) do
              begin
                  //Again it's cheaper to do the small cmd twice than do the large cmd
                  //once for data that can be handled by two small cmds.
                  //cmd 1???????
                  if (skipcount < XOR_MED) then
                  begin
                      if xorcount <= XOR_SMALL then count := byte(skipcount) else count := byte(XOR_SMALL);
                      dest[putp] := Byte((count or $80));
                      inc(putp);
                      //cmd 10000000 0??????? ????????
                  end
                  else
                  begin
                      if skipcount <= XOR_MAX then count := word(skipcount) else count := word(XOR_MAX);
                      dest[putp] := $80;
                      inc(putp);
                      dest[putp] := Byte((count and $FF));
                      inc(putp);
                      dest[putp] := Byte(((count shr 8) and $FF));
                      inc(putp);
                  end;
                     skipcount := skipcount- count;
                  getsp := getsp + count;
                  getbp := getbp + count;
             end;
          end;
           //final skip command of 0 to signal end of stream.
          dest[putp] := $80;
          inc(putp);
          dest[putp] := 0;
          inc(putp);
          dest[putp] := 0;
          inc(putp);
          //Byte[] finalOutput = new Byte[putp];
          // Array.Copy(dest, 0, finalOutput, 0, putp);
          // Return the final data
          result := putp;
end;

/// <summary>
      /// Applies a binary delta to a buffer.
      /// </summary>
      /// <param name="data">The data to apply the xor to.</param>
      /// <param name="xorSource">The the delta data to apply.</param>
      /// <param name="xorStart">Start offset in the data.</param>
      /// <param name="xorEnd">End offset in the data. Use 0 to take the end of the whole array.</param>
     // public static void ApplyXorDelta(Byte[] data, Byte[] xorSource, ref Int32 xorStart, Int32 xorEnd)
procedure ApplyXorDelta(xorSource: array of Byte; var data: array of Byte;   xorStart: Integer; xorEnd: Integer; DataEnd: Integer);
var
putp           : Integer = 0;
value          : byte    = 0;
cmd            : Byte;
count          : word;
xorval         : Boolean;
Begin
          dataEnd := DataEnd;
          if (xorEnd <= 0) then xorEnd := xorEnd;
          while (putp < dataEnd ) and (xorStart < xorEnd ) do
          begin
              //DEBUG_SAY("XOR_Delta Put pos: %u, Get pos: %u.... ", putp - scast<sint8*>(dest), getp - scast<sint8*>(source));
              cmd := xorSource[xorStart];
              Inc(xorStart);
              count := cmd;
              xorval := false;
              if ((cmd and $80) = 0) then
              begin
                  //0b00000000
                  if (cmd = 0) then
                  begin
                      if (xorStart >= xorEnd) then exit; //return;
                      count := word((xorSource[xorStart] and $FF));
                      inc(xorStart);
                      if (xorStart >= xorEnd) then exit;//  return;
                      value := xorSource[xorStart];
                      Inc(xorStart);
                      xorval := true;
                      //DEBUG_SAY("0b00000000 Val Count %d ", count);
                      //0b0???????
                  end;
              end
              else
                  begin
                  //0b1??????? remove most significant bit
                  count  := count and $7F;
                  if (count <> 0) then
                  begin
                      putp :=  putp + count;
                      //DEBUG_SAY("0b1??????? Skip Count %d\n", count);
                      continue;
                  end;
                  if (xorStart >= xorEnd) then exit; // return;
                  count := word((xorSource[xorStart] and $FF));
                  Inc(xorStart);
                  if (xorStart >= xorEnd) then exit; // return;
                  count := count + word((xorSource[xorStart] shl 8));
                  inc(xorStart);
                    //0b10000000 0 0
                  if (count = 0) then
                  begin
                      //DEBUG_SAY("0b10000000 Count %d to end delta\n", count);
                      exit; // return;
                  end;

                  //0b100000000 0?
                  if ((count and $8000) = 0) then
                  begin
                      putp := putp + count;
                      //DEBUG_SAY("0b100000000 0? Skip Count %d\n", count);
                      continue;
                  end;
                  //0b10000000 11
                  if ((count and $4000) <> 0) then
                  begin
                      count :=count and $3FFF;
                      if (xorStart >= xorEnd) then exit; // return;
                      value := xorSource[xorStart];
                      Inc(xorStart);
                      //DEBUG_SAY("0b10000000 11 Val Count %d ", count);
                      xorval := true;
                      //0b10000000 10
                  end
                  else
                      begin
                      count := count and $3FFF;
                      //DEBUG_SAY("0b10000000 10 XOR Count %d ", count);
                  end;
              end;

              if (xorval) then
              begin
                  //DEBUG_SAY("XOR Val %d\n", value);
                  while count > 0 do
                  begin
                       if (putp >= dataEnd) then exit; // return
                      data[putp]:= data[putp] XOR value;
                       inc(putp);
                       dec(count);
                  end;
              end
              else
              begin
                  //DEBUG_SAY("XOR Source to Dest\n");
                   while count >0 do
                   begin
                       if (putp >= dataEnd) Or (xorStart >= xorEnd) then exit; // return
                       data[putp]:= data[putp] XOR xorSource[xorStart];
                       dec(count);
                       Inc(putp);
                       Inc(xorStart);
                   end;
               end;
          end;


end;


      /// <summary>
      ///    Compresses data to the proprietary LCW format used in
      ///    many games developed by Westwood Studios. Compression is better
      ///    than that achieved by popular community tools. This is a new
      ///    implementation based on understanding of the compression gained from
      ///    the reference code.
      /// </summary>
      /// <param name="input">Array of the data to compress.</param>
      /// <variable>Output The compressed data array.</variable>
      /// result Integer is the count in the Output array +1
      /// <remarks>Commonly known in the community as "format80".</remarks>

function LcwCompress(input : array of byte; var OutPut: array of Byte):Integer;
var
relative ,cmd_one            : boolean;
getp,putp,i                  : Integer;
getend,offset                : Integer;
worstcase,rel_offset         : Integer;
cmd_onep,offchk,offsetp      : Integer;
rlemax,rlep,offstart         : Integer;
run_length,block_size        : Word;
begin
          if  ( High(input) = 0) then exit;


          //Decide if we are going to do relative offsets for 3 and 5 byte commands
           if High(input) > Word.MaxValue then relative := true  else  relative := false;

          // Nyer's C# conversion: replacements for write and read for pointers.
          getp := 0;
          putp := 0;
          // Input length. Used commonly enough to warrant getting it out in advance I guess.
          getend := High(input);
          // "Worst case length" code by OmniBlade. We'll just use a buffer of
          // that max length and cut it down to the actual used size at the end.
          // Not using it- it's not big enough in case of some small images.
          //LCWWorstCase(getend)
          worstcase := Max(10000, getend * 2);
          //  Byte[] output = new Byte[worstcase];  // not used since we are using static arrays

          // relative LCW starts with 0 as flag to decoder.
          // this is only used by later games for decoding hi-color vqa files.
          if (relative ) then  begin output[putp] := 0; inc(putp); end;

          //Implementations that properly conform to the WestWood encoder should
          //write a starting cmd1. It's important for using the offset copy commands
          //to do more efficient RLE in some cases than the cmd4.

          //we also set bool to flag that we have an on going cmd1.
          cmd_onep := putp;
          output[putp] := $81;   inc(putp);
          output[putp] := input[getp];   inc(putp);  inc(getP);
          cmd_one := true;

          //Compress data until we reach end of input buffer.
          while (getp < getend) do
          begin
              //Is RLE encode (4bytes) worth evaluating?
              if (getend - getp > 64 ) and ( input[getp] = input[getp + 64])  then
              begin
                  //RLE run length is encoded as a short so max is UINT16_MAX
                 if ((getend - getp) < Word.MaxValue) then rlemax := getend else rlemax := getp + Word.MaxValue;
                  rlep := getp + 1;
                  while (rlep < rlemax)  and (input[rlep] = input[getp]) do inc(rlep);


                  run_length := Word((rlep - getp));

                  //If run length is long enough, write the command and start loop again
                  if (run_length >= $41) then
                  begin
                     //write 4byte command 0b11111110
                      cmd_one := false;
                      output[putp] := $FE;
                      inc(putp);
                      output[putp] := Byte((run_length and $FF));
                      inc(putp);
                      output[putp] := Byte(((run_length shr 8) and $FF));
                      inc(putp);
                      output[putp] := input[getp];
                      inc(putp);
                      getp := rlep;
                      continue;
                  end;
              end;

             //current block size for an offset copy
               block_size := 0;
              //Set where we start looking for matching runs.
                  if (relative) and ( getp >= Word.MaxValue) then offstart := Word.MaxValue
                      else
                        offstart := 0;

              //Look for matching runs
              offchk := offstart;
              offsetp := getp;
              while (offchk < getp)do
              begin
                 //Move offchk to next matching position
                  while (offchk < getp ) and (input[offchk] <> input[getp]) do
                      Inc(offchk);

                  //If the checking pointer has reached current pos, break
                  if (offchk >= getp) then  break;

                  //find out how long the run of matches goes for
                     I:= 1;
                  while ( getp + i < getend) do
                   begin
                      if ((input[offchk + i]) <> input[getp + i]) then   break;
                      inc(i);
                   end;
                  if (i >= block_size)then
                  begin
                      block_size := Word(i);
                      offsetp := offchk;
                  end;
                  inc(offchk);
              end;

              //decide what encoding to use for current run
              //If it's less than 2 bytes long, we store as is with cmd1
              if (block_size <= 2) then
              begin
                  //short copy 0b10??????
                  //check we have an existing 1 byte command and if its value is still
                  //small enough to handle additional bytes
                  //start a new command if current one doesn't have space or we don't
                  //have one to continue
                  if (cmd_one  ) and (output[cmd_onep] < $BF) then
                  begin
                      //increment command value
                      inc(output[cmd_onep]);
                      output[putp] := input[getp];
                      inc(putp); Inc(getp);
                  end
                 else
                  begin
                      cmd_onep := putp;
                      output[putp] := $81; inc(putp);
                      output[putp] := input[getp]; inc(putp); inc(getp);
                      cmd_one := true;
                  end;
              //Otherwise we need to decide what relative copy command is most efficient
              end
             else
              begin
                   rel_offset := getp - offsetp;
                  if (block_size > $A )or ((rel_offset) > $FFF) then
                  begin
                      //write 5 byte command 0b11111111
                      if (block_size > $40) then
                      begin
                          output[putp] := $FF; inc(putp);
                          output[putp] := Byte((block_size and $FF)); inc(putp);
                          output[putp] := Byte(((block_size shr 8) and $FF)); inc(putp);
                          //write 3 byte command 0b11??????
                       end
                     else
                      begin
                          output[putp] := Byte(((block_size - 3) or $C0));  inc(putp);
                      end;
                       if relative then offset:= rel_offset else Offset:= Offsetp;
                       //write 2 byte command? 0b0???????
                  end
                 else
                  begin
                      offset := (rel_offset shl 8) or (16 * (block_size - 3) + (rel_offset shr 8));
                  end;
                  output[putp] := Byte((offset and $FF));   inc(putp);
                  output[putp] := Byte(((offset shr 8) and $FF));  inc(putp);
                  getp := getp+ block_size;
                  cmd_one := false;
              end;
          end;

          //write final $80, basically an empty cmd1 to signal the end of the stream.
          output[putp] := $80;  inc(putp);
          result := Putp;  // returned size is +1

end;

end.

