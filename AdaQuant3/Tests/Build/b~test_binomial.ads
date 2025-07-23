pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2009 (20090519)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test_binomial" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure Break_Start;
   pragma Import (C, Break_Start, "__gnat_break_start");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#7956ec76#;
   u00002 : constant Version_32 := 16#6385d640#;
   u00003 : constant Version_32 := 16#e913139a#;
   u00004 : constant Version_32 := 16#9c7dd3ea#;
   u00005 : constant Version_32 := 16#4e66f673#;
   u00006 : constant Version_32 := 16#776f339c#;
   u00007 : constant Version_32 := 16#79f6a4e0#;
   u00008 : constant Version_32 := 16#c485acd5#;
   u00009 : constant Version_32 := 16#ba011fb9#;
   u00010 : constant Version_32 := 16#e3d85f73#;
   u00011 : constant Version_32 := 16#244fbd22#;
   u00012 : constant Version_32 := 16#63a35e59#;
   u00013 : constant Version_32 := 16#b8febf89#;
   u00014 : constant Version_32 := 16#2ea84b20#;
   u00015 : constant Version_32 := 16#d761ebcb#;
   u00016 : constant Version_32 := 16#18c4823a#;
   u00017 : constant Version_32 := 16#525996ff#;
   u00018 : constant Version_32 := 16#bcda8803#;
   u00019 : constant Version_32 := 16#809ac104#;
   u00020 : constant Version_32 := 16#892f4d5b#;
   u00021 : constant Version_32 := 16#c8855f99#;
   u00022 : constant Version_32 := 16#2f60aa04#;
   u00023 : constant Version_32 := 16#f38c5b75#;
   u00024 : constant Version_32 := 16#e43c4f3d#;
   u00025 : constant Version_32 := 16#7cfa671a#;
   u00026 : constant Version_32 := 16#ebfc0b85#;
   u00027 : constant Version_32 := 16#4caa1034#;
   u00028 : constant Version_32 := 16#88c8686c#;
   u00029 : constant Version_32 := 16#6010f2e6#;
   u00030 : constant Version_32 := 16#6997f8be#;
   u00031 : constant Version_32 := 16#a7085568#;
   u00032 : constant Version_32 := 16#e4b647c7#;
   u00033 : constant Version_32 := 16#08a5f9f2#;
   u00034 : constant Version_32 := 16#148b30e3#;
   u00035 : constant Version_32 := 16#776b72d1#;
   u00036 : constant Version_32 := 16#90fc7f12#;
   u00037 : constant Version_32 := 16#906233be#;
   u00038 : constant Version_32 := 16#bf6b7708#;
   u00039 : constant Version_32 := 16#a69cad5c#;
   u00040 : constant Version_32 := 16#093802d2#;
   u00041 : constant Version_32 := 16#37f831b2#;
   u00042 : constant Version_32 := 16#fe5e1c6e#;
   u00043 : constant Version_32 := 16#82214f42#;
   u00044 : constant Version_32 := 16#a8d17654#;
   u00045 : constant Version_32 := 16#0a7e05c0#;
   u00046 : constant Version_32 := 16#a34766a8#;
   u00047 : constant Version_32 := 16#647de85b#;
   u00048 : constant Version_32 := 16#05890af9#;
   u00049 : constant Version_32 := 16#0d0a8d0c#;
   u00050 : constant Version_32 := 16#7e61c04e#;
   u00051 : constant Version_32 := 16#895f8c1e#;
   u00052 : constant Version_32 := 16#3acc97b8#;
   u00053 : constant Version_32 := 16#62e56d2b#;
   u00054 : constant Version_32 := 16#ced5a363#;
   u00055 : constant Version_32 := 16#5ec8815d#;
   u00056 : constant Version_32 := 16#f1de4b60#;
   u00057 : constant Version_32 := 16#0ca0c359#;
   u00058 : constant Version_32 := 16#1a3a7ed3#;
   u00059 : constant Version_32 := 16#37a7e042#;
   u00060 : constant Version_32 := 16#6d0998e1#;
   u00061 : constant Version_32 := 16#550b7cbf#;
   u00062 : constant Version_32 := 16#a0a00019#;
   u00063 : constant Version_32 := 16#558f3db1#;
   u00064 : constant Version_32 := 16#293ff6f7#;
   u00065 : constant Version_32 := 16#2ab25649#;
   u00066 : constant Version_32 := 16#2461b049#;
   u00067 : constant Version_32 := 16#0aa29e81#;
   u00068 : constant Version_32 := 16#2274d34a#;
   u00069 : constant Version_32 := 16#59507545#;
   u00070 : constant Version_32 := 16#e98c0dd7#;
   u00071 : constant Version_32 := 16#93c54a02#;
   u00072 : constant Version_32 := 16#a0aa86d6#;
   u00073 : constant Version_32 := 16#cc47afb0#;
   u00074 : constant Version_32 := 16#ecf0bdc3#;
   u00075 : constant Version_32 := 16#f31c9746#;
   u00076 : constant Version_32 := 16#4be846ff#;
   u00077 : constant Version_32 := 16#e42a19e8#;
   u00078 : constant Version_32 := 16#a00dd918#;
   u00079 : constant Version_32 := 16#64b33ff2#;
   u00080 : constant Version_32 := 16#808e35e2#;
   u00081 : constant Version_32 := 16#45aed95a#;
   u00082 : constant Version_32 := 16#3cdf3a90#;
   u00083 : constant Version_32 := 16#c492e5c1#;
   u00084 : constant Version_32 := 16#41d98856#;
   u00085 : constant Version_32 := 16#e0683b80#;
   u00086 : constant Version_32 := 16#639d3a29#;
   u00087 : constant Version_32 := 16#512b6dd4#;
   u00088 : constant Version_32 := 16#607d1d10#;
   u00089 : constant Version_32 := 16#59fe1f35#;
   u00090 : constant Version_32 := 16#40fa1a90#;
   u00091 : constant Version_32 := 16#139318f2#;
   u00092 : constant Version_32 := 16#cd5dea71#;
   u00093 : constant Version_32 := 16#574aad9d#;
   u00094 : constant Version_32 := 16#80e564ae#;
   u00095 : constant Version_32 := 16#19674ef8#;
   u00096 : constant Version_32 := 16#0255db5c#;
   u00097 : constant Version_32 := 16#d332ae1e#;
   u00098 : constant Version_32 := 16#8d02aab0#;
   u00099 : constant Version_32 := 16#fec70fdd#;
   u00100 : constant Version_32 := 16#3e7d115b#;
   u00101 : constant Version_32 := 16#291cb31a#;
   u00102 : constant Version_32 := 16#9b936ce6#;
   u00103 : constant Version_32 := 16#10a30cee#;
   u00104 : constant Version_32 := 16#04905426#;
   u00105 : constant Version_32 := 16#7e93ed41#;
   u00106 : constant Version_32 := 16#e2ccbb91#;

   pragma Export (C, u00001, "test_binomialB");
   pragma Export (C, u00002, "system__standard_libraryB");
   pragma Export (C, u00003, "system__standard_libraryS");
   pragma Export (C, u00004, "adaS");
   pragma Export (C, u00005, "ada__text_ioB");
   pragma Export (C, u00006, "ada__text_ioS");
   pragma Export (C, u00007, "ada__exceptionsB");
   pragma Export (C, u00008, "ada__exceptionsS");
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   pragma Export (C, u00011, "systemS");
   pragma Export (C, u00012, "system__soft_linksB");
   pragma Export (C, u00013, "system__soft_linksS");
   pragma Export (C, u00014, "system__parametersB");
   pragma Export (C, u00015, "system__parametersS");
   pragma Export (C, u00016, "system__secondary_stackB");
   pragma Export (C, u00017, "system__secondary_stackS");
   pragma Export (C, u00018, "system__storage_elementsB");
   pragma Export (C, u00019, "system__storage_elementsS");
   pragma Export (C, u00020, "system__stack_checkingB");
   pragma Export (C, u00021, "system__stack_checkingS");
   pragma Export (C, u00022, "system__exception_tableB");
   pragma Export (C, u00023, "system__exception_tableS");
   pragma Export (C, u00024, "system__htableB");
   pragma Export (C, u00025, "system__htableS");
   pragma Export (C, u00026, "system__string_hashB");
   pragma Export (C, u00027, "system__string_hashS");
   pragma Export (C, u00028, "system__exceptionsB");
   pragma Export (C, u00029, "system__exceptionsS");
   pragma Export (C, u00030, "system__tracebackB");
   pragma Export (C, u00031, "system__tracebackS");
   pragma Export (C, u00032, "system__unsigned_typesS");
   pragma Export (C, u00033, "system__wch_conB");
   pragma Export (C, u00034, "system__wch_conS");
   pragma Export (C, u00035, "system__wch_stwB");
   pragma Export (C, u00036, "system__wch_stwS");
   pragma Export (C, u00037, "system__wch_cnvB");
   pragma Export (C, u00038, "system__wch_cnvS");
   pragma Export (C, u00039, "interfacesS");
   pragma Export (C, u00040, "system__wch_jisB");
   pragma Export (C, u00041, "system__wch_jisS");
   pragma Export (C, u00042, "system__traceback_entriesB");
   pragma Export (C, u00043, "system__traceback_entriesS");
   pragma Export (C, u00044, "ada__streamsS");
   pragma Export (C, u00045, "ada__tagsB");
   pragma Export (C, u00046, "ada__tagsS");
   pragma Export (C, u00047, "system__val_unsB");
   pragma Export (C, u00048, "system__val_unsS");
   pragma Export (C, u00049, "system__val_utilB");
   pragma Export (C, u00050, "system__val_utilS");
   pragma Export (C, u00051, "system__case_utilB");
   pragma Export (C, u00052, "system__case_utilS");
   pragma Export (C, u00053, "interfaces__c_streamsB");
   pragma Export (C, u00054, "interfaces__c_streamsS");
   pragma Export (C, u00055, "system__crtlS");
   pragma Export (C, u00056, "system__file_ioB");
   pragma Export (C, u00057, "system__file_ioS");
   pragma Export (C, u00058, "ada__finalizationB");
   pragma Export (C, u00059, "ada__finalizationS");
   pragma Export (C, u00060, "system__finalization_rootB");
   pragma Export (C, u00061, "system__finalization_rootS");
   pragma Export (C, u00062, "system__finalization_implementationB");
   pragma Export (C, u00063, "system__finalization_implementationS");
   pragma Export (C, u00064, "system__restrictionsB");
   pragma Export (C, u00065, "system__restrictionsS");
   pragma Export (C, u00066, "system__stream_attributesB");
   pragma Export (C, u00067, "system__stream_attributesS");
   pragma Export (C, u00068, "ada__io_exceptionsS");
   pragma Export (C, u00069, "interfaces__cB");
   pragma Export (C, u00070, "interfaces__cS");
   pragma Export (C, u00071, "system__os_libB");
   pragma Export (C, u00072, "system__os_libS");
   pragma Export (C, u00073, "system__stringsB");
   pragma Export (C, u00074, "system__stringsS");
   pragma Export (C, u00075, "system__file_control_blockS");
   pragma Export (C, u00076, "ada__finalization__list_controllerB");
   pragma Export (C, u00077, "ada__finalization__list_controllerS");
   pragma Export (C, u00078, "mathsB");
   pragma Export (C, u00079, "mathsS");
   pragma Export (C, u00080, "ada__numericsS");
   pragma Export (C, u00081, "ada__numerics__auxB");
   pragma Export (C, u00082, "ada__numerics__auxS");
   pragma Export (C, u00083, "system__fat_llfS");
   pragma Export (C, u00084, "system__machine_codeS");
   pragma Export (C, u00085, "system__exn_llfB");
   pragma Export (C, u00086, "system__exn_llfS");
   pragma Export (C, u00087, "system__fat_fltS");
   pragma Export (C, u00088, "interfaces__fortranB");
   pragma Export (C, u00089, "interfaces__fortranS");
   pragma Export (C, u00090, "system__fat_lfltS");
   pragma Export (C, u00091, "interfaces__fortran__lapackS");
   pragma Export (C, u00092, "interfaces__fortran__blasB");
   pragma Export (C, u00093, "interfaces__fortran__blasS");
   pragma Export (C, u00094, "system__concat_2B");
   pragma Export (C, u00095, "system__concat_2S");
   pragma Export (C, u00096, "system__img_intB");
   pragma Export (C, u00097, "system__img_intS");
   pragma Export (C, u00098, "system__img_realB");
   pragma Export (C, u00099, "system__img_realS");
   pragma Export (C, u00100, "system__img_lluB");
   pragma Export (C, u00101, "system__img_lluS");
   pragma Export (C, u00102, "system__img_unsB");
   pragma Export (C, u00103, "system__img_unsS");
   pragma Export (C, u00104, "system__powten_tableS");
   pragma Export (C, u00105, "system__memoryB");
   pragma Export (C, u00106, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.htable%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_real%s
   --  system.machine_code%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.secondary_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.last_chance_handler%s
   --  system.soft_links%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.secondary_stack%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.aux%s
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.unsigned_types%s
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  interfaces.fortran%s
   --  interfaces.fortran%b
   --  interfaces.fortran.blas%s
   --  interfaces.fortran.blas%b
   --  interfaces.fortran.lapack%s
   --  system.fat_llf%s
   --  ada.numerics.aux%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_real%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.finalization_implementation%s
   --  system.finalization_implementation%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  ada.finalization.list_controller%s
   --  ada.finalization.list_controller%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.tags%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  maths%s
   --  maths%b
   --  test_binomial%b
   --  END ELABORATION ORDER

end ada_main;
