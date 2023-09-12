##################
Example C++ plugin
##################

main.h
------

.. code-block:: c++

  #ifndef __MAIN_H__
  #define __MAIN_H__

  #include <cstdint>

  #if defined(_WIN32) || defined(_WIN64)
  #define EXPORT __declspec(dllexport)
  #else
  #define EXPORT [[gnu::visibility("default")]]
  #endif

  #if INTPTR_MAX == INT32_MAX
      typedef int32_t NativeInt;
      typedef uint32_t NativeUInt;
  #else
      typedef int64_t NativeInt;
      typedef uint64_t NativeUInt;
  #endif

  struct __attribute__((__packed__)) TSimbaInfo
  {
      int SimbaVersion;
      int SimbaMajor;

      char* FileName;

      void* Compiler;
  };

  struct __attribute__((__packed__)) TSimbaMethods
  {
      void (*RunOnMainThread)(void(*Method)(void*), void* data);

      void* (*GetMem)(NativeUInt size);
      void  (*FreeMem)(void* ptr);
      void* (*AllocMem)(NativeUInt Size);
      void* (*ReAllocMem)(void** ptr, NativeUInt size);
      NativeUInt (*MemSize)(void* ptr);

      void  (*RaiseException)(char* Message);
      void* (*GetTypeInfo)(void* Compiler, char* Typ);
      NativeUInt (*GetTypeInfoSize)(void* TypeInfo);
      NativeInt  (*GetTypeInfoFieldOffset)(void* TypeInfo, char* FieldName);

      void* (*AllocateRawArray)(NativeInt ElementSize, NativeUInt Len);
      void (*ReAllocateRawArray)(void** ptr, NativeInt ElementSize, NativeUInt Len);

      void* (*AllocateArray)(void* TypeInfo, NativeUInt Len);
      void* (*AllocateString)(void* Data);
      void* (*AllocateUnicodeString)(void* Data);

      void (*SetArrayLength)(void* TypeInfo, void**ptr, NativeInt NewLen);
      NativeInt (*GetArrayLength)(void* AVar);
  };

  TSimbaInfo* SIMBA_INFO = {0};
  TSimbaMethods* SIMBA_METHODS = {0};

  extern "C"
  {
    EXPORT int GetTypeCount();
    EXPORT int GetTypeInfo(int Index, char** Name, char** Definition);
    EXPORT int GetFunctionCount();
    EXPORT int GetFunctionInfo(int Index, void** Address, char** Definition);
    EXPORT void RegisterSimbaPlugin(TSimbaInfo* Information, TSimbaMethods* Methods);
  }

  #endif // __MAIN_H__

main.cpp
--------

.. code-block:: c++

  #include "main.h"
  #include "stdio.h"
  #include <cstring>

  template<typename T>
  void MemWrite(void* ptr, int offset, T item) noexcept
  {
      memcpy((char*)ptr+offset, &item, sizeof(T));
  }

  template<typename T>
  T MemRead(void* ptr) noexcept
  {
      T result;
      memcpy(&result, ptr, sizeof(T));
      return result;
  }

  void* ARR_TYPEINFO = 0;
  void* REC_TYPEINFO = 0;
  NativeUInt REC_SIZE = 0;
  NativeUInt REC_STR_OFFSET = 0;

  void GetIntArray(void** Params, void** Result)
  {
      int Count = MemRead<int>(*Params);
      void* Arr = SIMBA_METHODS->AllocateRawArray(sizeof(int32_t), Count);
      for (int i=0; i<Count; i++) {
          MemWrite(Arr, i*sizeof(int), i);
      }
      MemWrite<void*>(Result, 0, Arr);
  }

  void GetRecord(void** Params, void** Result)
  {
      MemWrite<int>(Result, 0, 123456);
      MemWrite<void*>(Result, REC_STR_OFFSET, SIMBA_METHODS->AllocateString((char*)"Hello world"));
  }

  void GetArrayOfRecord(void** Params, void** Result)
  {
      char str0[] = "Hello in array index 0";
      char str1[] = "Hola in array index 1";
      char str2[] = "Bonjour in array index 2";

      void* mem = SIMBA_METHODS->AllocateArray(ARR_TYPEINFO, 3);
      for (int i=0; i<3; i++) {
          void* str = nullptr;
          switch (i) {
              case 0: str = SIMBA_METHODS->AllocateString((void*)str0); break;
              case 1: str = SIMBA_METHODS->AllocateString((void*)str1); break;
              case 2: str = SIMBA_METHODS->AllocateString((void*)str2); break;
          }

          // write arr[i].i
          MemWrite<int>(mem, i*REC_SIZE, i);
          // write  arr[i].str
          MemWrite<void*>(mem, (i*REC_SIZE)+REC_STR_OFFSET, str);
      }

      MemWrite<void*>(Result, 0, mem);
  }

  void RegisterSimbaPlugin(TSimbaInfo* Info, TSimbaMethods* Methods)
  {
      SIMBA_INFO = Info;
      SIMBA_METHODS = Methods;

      REC_TYPEINFO = SIMBA_METHODS->GetTypeInfo(SIMBA_INFO->Compiler, (char*)"TMyRecord");
      REC_SIZE = SIMBA_METHODS->GetTypeInfoSize(REC_TYPEINFO);
      REC_STR_OFFSET = SIMBA_METHODS->GetTypeInfoFieldOffset(REC_TYPEINFO, (char*)"str");

      ARR_TYPEINFO = SIMBA_METHODS->GetTypeInfo(SIMBA_INFO->Compiler, (char*)"array of TMyRecord");
  }

  int GetTypeCount()
  {
      return 1;
  }

  int GetTypeInfo(int Index, char** Name, char** Definition)
  {
      switch(Index) {
          case 0:
              strcpy(*Name, "TMyRecord");
              strcpy(*Definition, "record i: Int32; str: String; end;");
              break;
      }
      return Index;
  }

  int GetFunctionCount()
  {
      return 3;
  }

  int GetFunctionInfo(int Index, void** Address, char** Definition)
  {
      switch(Index) {
          case 0:
              strcpy(*Definition, "function GetIntArray(Count: Int32): array of Int32; native;");
              *Address = (void*)GetIntArray;
              break;

          case 1:
              strcpy(*Definition, "function GetRecord: TMyRecord; native;");
              *Address = (void*)GetRecord;
              break;

          case 2:
              strcpy(*Definition, "function GetArrayOfRecord: array of TMyRecord; native;");
              *Address = (void*)GetArrayOfRecord;
              break;
      }

      return Index;
  }


