static void check_object(ptr p, uptr seg, ISPC s_in, IBOOL aftergc)
{
  ITYPE t = TYPEBITS(p);
  if (t == type_typed_object)
  {
    ptr tf = TYPEFIELD(p);
    if (TYPEP(tf, mask_record, type_record))
    {
      check_pointer(&(RECORDINSTTYPE(p)), 1, 0, p, seg, s_in, aftergc);
      {
        ptr rtd = RECORDINSTTYPE(p);
        {
          uptr len = UNFIX((RECORDDESCSIZE(rtd)));
          {
            ptr num = RECORDDESCPM(rtd);
            ptr* pp = &(RECORDINSTIT(p, 0));
            if (Sfixnump(num))
            {
              {
                uptr mask = ((uptr)(UNFIX(num))) >> 1;
                if (mask == (((uptr)-1) >> 1))
                {
                  {
                    ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                    while (pp < ppend)
                    {
                      check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                      pp += 1;
                    }
                  }
                }
                else
                {
                  while (mask != 0)
                  {
                    if (mask & 1)
                    {
                      check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                    }
                    mask >>= 1;
                    pp += 1;
                  }
                }
              }
            }
            else if (num == Strue)
            {
              uptr offset = (uptr)(*((pp + 1)));
              {
                ptr obj = TO_PTR((((uptr)(*(pp))) - offset));
                check_pointer(&(obj), 0, 0, p, seg, s_in, aftergc);
                *(pp) = TO_PTR((((uptr)obj) + offset));
              }
            }
            else
            {
              check_pointer(&(num), 0, 0, p, seg, s_in, aftergc);
              check_bignum(num);
              {
                iptr index = (BIGLEN(num)) - 1;
                bigit mask = (BIGIT(num, index)) >> 1;
                INT bits = bigit_bits - 1;
                while (1)
                {
                  do
                  {
                    if (mask & 1)
                    {
                      check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                    }
                    mask >>= 1;
                    pp += 1;
                    bits -= 1;
                  }
                  while (bits > 0);
                  if (index == 0)
                  {
                    break;
                  }
                  index -= 1;
                  mask = BIGIT(num, index);
                  bits = bigit_bits;
                }
              }
            }
          }
        }
      }
    }
    else if (TYPEP(tf, mask_vector, type_vector))
    {
      uptr len = Svector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          check_pointer(&(p_p[idx]), 1, 0, p, seg, s_in, aftergc);
        }
      }
    }
    else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
    {
      uptr len = Sstencil_vector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITSTENVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          check_pointer(&(p_p[idx]), 1, 0, p, seg, s_in, aftergc);
        }
      }
    }
    else if (TYPEP(tf, mask_string, type_string))
    {
    }
    else if (TYPEP(tf, mask_fxvector, type_fxvector))
    {
    }
    else if (TYPEP(tf, mask_flvector, type_flvector))
    {
    }
    else if (TYPEP(tf, mask_bytevector, type_bytevector))
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_reference_array)
        {
          {
            uptr len = Sbytevector_reference_length(p);
            {
              uptr idx, p_len = len;
              ptr *p_p = (ptr*)&BVIT(p, 0);
              for (idx = 0; idx < p_len; idx++)
              {
                check_pointer(&(p_p[idx]), 1, 1, p, seg, s_in, aftergc);
              }
            }
          }
        }
        else
        {
        }
      }
    }
    else if ((iptr)tf == type_tlc)
    {
      check_pointer(&(INITTLCHT(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(INITTLCKEYVAL(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(INITTLCNEXT(p)), 1, 0, p, seg, s_in, aftergc);
    }
    else if (TYPEP(tf, mask_box, type_box))
    {
      check_pointer(&(INITBOXREF(p)), 1, 0, p, seg, s_in, aftergc);
    }
    else if ((iptr)tf == type_ratnum)
    {
      check_pointer(&(RATNUM(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(RATDEN(p)), 1, 0, p, seg, s_in, aftergc);
    }
    else if ((iptr)tf == type_exactnum)
    {
      check_pointer(&(EXACTNUM_REAL_PART(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(EXACTNUM_IMAG_PART(p)), 1, 0, p, seg, s_in, aftergc);
    }
    else if ((iptr)tf == type_inexactnum)
    {
    }
    else if (TYPEP(tf, mask_bignum, type_bignum))
    {
    }
    else if (TYPEP(tf, mask_port, type_port))
    {
      check_pointer(&(PORTHANDLER(p)), 1, 0, p, seg, s_in, aftergc);
      if (((uptr)tf) & PORT_FLAG_OUTPUT)
      {
        check_pointer(&(PORTOBUF(p)), 1, 0, p, seg, s_in, aftergc);
      }
      if (((uptr)tf) & PORT_FLAG_INPUT)
      {
        check_pointer(&(PORTIBUF(p)), 1, 0, p, seg, s_in, aftergc);
      }
      check_pointer(&(PORTINFO(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(PORTNAME(p)), 1, 0, p, seg, s_in, aftergc);
    }
    else if (TYPEP(tf, mask_code, type_code))
    {
      check_pointer(&(CODENAME(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(CODEARITYMASK(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(CODEINFO(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(CODEPINFOS(p)), 1, 0, p, seg, s_in, aftergc);
      {
        ptr t = CODERELOC(p);
        {
          iptr m = (t
                    ? (RELOCSIZE(t))
                    : 0);
          {
            ptr oldco = (t
                         ? (RELOCCODE(t))
                         : 0);
            {
              iptr a = 0;
              {
                iptr n = 0;
                while (n < m)
                {
                  {
                    uptr entry = RELOCIT(t, n);
                    uptr item_off = 0;
                    uptr code_off = 0;
                    n = n + 1;
                    if (RELOC_EXTENDED_FORMAT(entry))
                    {
                      item_off = RELOCIT(t, n);
                      n = n + 1;
                      code_off = RELOCIT(t, n);
                      n = n + 1;
                    }
                    else
                    {
                      item_off = RELOC_ITEM_OFFSET(entry);
                      code_off = RELOC_CODE_OFFSET(entry);
                    }
                    a = a + code_off;
                    {
                      ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                      check_pointer(&(obj), 0, 0, p, seg, s_in, aftergc);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_thread)
    {
      {
        ptr tc = (ptr)(THREADTC(p));
        if (tc != ((ptr)0))
        {
          STACKCACHE(tc) = Snil;
          check_pointer(&(CCHAIN(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(STACKLINK(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(WINDERS(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(ATTACHMENTS(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(HANDLERSTACK(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(CURRENTNATIVEFIBER(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(FIBERSWITCHPROHIBITEDDEPTH(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERCLAIMED(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERCLAIMEDCONTROL(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERPINNEDHEAD(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERTESTACTIVE(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERPREEMPTTARGET(tc)), 1, 0, p, seg, s_in, aftergc);
          check_pointer(&(NATIVEFIBERPREEMPTPAYLOAD(tc)), 1, 0, p, seg, s_in, aftergc);
          {
            ptr xcp = FRAME(tc, 0);
            {
              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
              {
                ptr c_p = (ptr)(((uptr)xcp) - co);
                check_pointer(&(c_p), 0, 0, p, seg, s_in, aftergc);
                {
                  uptr base = (uptr)(SCHEMESTACK(tc));
                  {
                    uptr fp = (uptr)(SFP(tc));
                    {
                      uptr ret = (uptr)(FRAME(tc, 0));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                check_pointer(&(c_p), 0, 0, p, seg, s_in, aftergc);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  check_pointer(&(num), 0, 0, p, seg, s_in, aftergc);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      check_pointer(&(THREADNO(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(CURRENTINPUT(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(CURRENTOUTPUT(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(CURRENTERROR(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(SFD(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(CURRENTMSO(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(TARGETMACHINE(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(FXLENGTHBV(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(FXFIRSTBITSETBV(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(COMPILEPROFILE(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(SUBSETMODE(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(DEFAULTRECORDEQUALPROCEDURE(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(DEFAULTRECORDHASHPROCEDURE(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(COMPRESSFORMAT(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(COMPRESSLEVEL(tc)), 1, 0, p, seg, s_in, aftergc);
                      check_pointer(&(PARAMETERS(tc)), 1, 0, p, seg, s_in, aftergc);
                      {
                        INT i = 0;
                        while (i < virtual_register_count)
                        {
                          check_pointer(&(VIRTREG(tc, i)), 1, 0, p, seg, s_in, aftergc);
                          i += 1;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_rtd_counts)
    {
    }
    else if ((iptr)tf == type_phantom)
    {
    }
    else
    {
      S_error_abort("check: illegal typed object type");
    }
  }
  else if (t == type_pair)
  {
    {
      ISPC p_at_spc = SPACE(p);
      if (p_at_spc < space_weakpair)
      {
        check_pointer(&(INITCAR(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITCDR(p)), 1, 0, p, seg, s_in, aftergc);
      }
      else if (p_at_spc == space_ephemeron)
      {
        check_pointer(&(INITCAR(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITCDR(p)), 1, 0, p, seg, s_in, aftergc);
      }
      else if (p_at_spc == space_weakpair)
      {
        check_pointer(&(INITCAR(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITCDR(p)), 1, 0, p, seg, s_in, aftergc);
      }
      else
      {
        S_error_abort("misplaced pair");
      }
    }
  }
  else if (t == type_closure)
  {
    ptr code = CLOSCODE(p);
    check_pointer(&(code), 0, 0, p, seg, s_in, aftergc);
    if ((GC_CODE_TYPE(code)) & (code_flag_continuation << code_flags_offset))
    {
      check_continuation_layout(p);
      check_pointer(&(CONTWINDERS(p)), 1, 0, p, seg, s_in, aftergc);
      check_pointer(&(CONTATTACHMENTS(p)), 1, 0, p, seg, s_in, aftergc);
      if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
      {
      }
      else
      {
        check_pointer(&(CONTLINK(p)), 1, 0, p, seg, s_in, aftergc);
        {
          ptr xcp = CONTRET(p);
          {
            iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
            {
              ptr c_p = (ptr)(((uptr)xcp) - co);
              check_pointer(&(c_p), 0, 0, p, seg, s_in, aftergc);
              {
                uptr stack = (uptr)(CONTSTACK(p));
                {
                  uptr base = stack;
                  {
                    uptr fp = stack + (CONTCLENGTH(p));
                    {
                      uptr ret = (uptr)(CONTRET(p));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                check_pointer(&(c_p), 0, 0, p, seg, s_in, aftergc);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  check_pointer(&(num), 0, 0, p, seg, s_in, aftergc);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            check_pointer(&(*(pp)), 1, 0, p, seg, s_in, aftergc);
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else
    {
      uptr len = CODEFREE(code);
      {
        uptr idx, p_len = len;
        ptr *p_p = &CLOSIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          check_pointer(&(p_p[idx]), 1, 0, p, seg, s_in, aftergc);
        }
      }
    }
  }
  else if (t == type_symbol)
  {
    check_pointer(&(INITSYMVAL(p)), 1, 0, p, seg, s_in, aftergc);
    {
      ptr val = INITSYMVAL(p);
      {
        ptr code = ((Sprocedurep(val))
                    ? (CLOSCODE(val))
                    : (SYMCODE(p)));
        check_pointer(&(code), 0, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITSYMPLIST(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITSYMNAME(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITSYMSPLIST(p)), 1, 0, p, seg, s_in, aftergc);
        check_pointer(&(INITSYMHASH(p)), 1, 0, p, seg, s_in, aftergc);
      }
    }
  }
  else if (t == type_flonum)
  {
  }
  else
  {
    S_error_abort("check: illegal type");
  }
}

static void shadow_object(ptr p)
{
  ITYPE t = TYPEBITS(p);
  if (t == type_typed_object)
  {
    ptr tf = TYPEFIELD(p);
    if (TYPEP(tf, mask_record, type_record))
    {
      shadow_pointer(RECORDINSTTYPE(p));
      {
        ptr rtd = RECORDINSTTYPE(p);
        {
          uptr len = UNFIX((RECORDDESCSIZE(rtd)));
          {
            ptr num = RECORDDESCPM(rtd);
            ptr* pp = &(RECORDINSTIT(p, 0));
            if (Sfixnump(num))
            {
              {
                uptr mask = ((uptr)(UNFIX(num))) >> 1;
                if (mask == (((uptr)-1) >> 1))
                {
                  {
                    ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                    while (pp < ppend)
                    {
                      shadow_pointer(*(pp));
                      pp += 1;
                    }
                  }
                }
                else
                {
                  while (mask != 0)
                  {
                    if (mask & 1)
                    {
                      shadow_pointer(*(pp));
                    }
                    mask >>= 1;
                    pp += 1;
                  }
                }
              }
            }
            else if (num == Strue)
            {
              uptr offset = (uptr)(*((pp + 1)));
              {
                ptr obj = TO_PTR((((uptr)(*(pp))) - offset));
                shadow_pointer(obj);
                *(pp) = TO_PTR((((uptr)obj) + offset));
              }
            }
            else
            {
              shadow_pointer(num);
              check_bignum(num);
              {
                iptr index = (BIGLEN(num)) - 1;
                bigit mask = (BIGIT(num, index)) >> 1;
                INT bits = bigit_bits - 1;
                while (1)
                {
                  do
                  {
                    if (mask & 1)
                    {
                      shadow_pointer(*(pp));
                    }
                    mask >>= 1;
                    pp += 1;
                    bits -= 1;
                  }
                  while (bits > 0);
                  if (index == 0)
                  {
                    break;
                  }
                  index -= 1;
                  mask = BIGIT(num, index);
                  bits = bigit_bits;
                }
              }
            }
          }
        }
      }
    }
    else if (TYPEP(tf, mask_vector, type_vector))
    {
      uptr len = Svector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_pointer(p_p[idx]);
        }
      }
    }
    else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
    {
      uptr len = Sstencil_vector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITSTENVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_pointer(p_p[idx]);
        }
      }
    }
    else if (TYPEP(tf, mask_string, type_string))
    {
    }
    else if (TYPEP(tf, mask_fxvector, type_fxvector))
    {
    }
    else if (TYPEP(tf, mask_flvector, type_flvector))
    {
    }
    else if (TYPEP(tf, mask_bytevector, type_bytevector))
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_reference_array)
        {
          {
            uptr len = Sbytevector_reference_length(p);
            {
              uptr idx, p_len = len;
              ptr *p_p = (ptr*)&BVIT(p, 0);
              for (idx = 0; idx < p_len; idx++)
              {
                shadow_pointer(S_maybe_reference_to_object(p_p[idx]));
              }
            }
          }
        }
        else
        {
        }
      }
    }
    else if ((iptr)tf == type_tlc)
    {
      shadow_pointer(INITTLCHT(p));
      shadow_pointer(INITTLCKEYVAL(p));
      shadow_pointer(INITTLCNEXT(p));
    }
    else if (TYPEP(tf, mask_box, type_box))
    {
      shadow_pointer(INITBOXREF(p));
    }
    else if ((iptr)tf == type_ratnum)
    {
      shadow_pointer(RATNUM(p));
      shadow_pointer(RATDEN(p));
    }
    else if ((iptr)tf == type_exactnum)
    {
      shadow_pointer(EXACTNUM_REAL_PART(p));
      shadow_pointer(EXACTNUM_IMAG_PART(p));
    }
    else if ((iptr)tf == type_inexactnum)
    {
    }
    else if (TYPEP(tf, mask_bignum, type_bignum))
    {
    }
    else if (TYPEP(tf, mask_port, type_port))
    {
      shadow_pointer(PORTHANDLER(p));
      if (((uptr)tf) & PORT_FLAG_OUTPUT)
      {
        shadow_pointer(PORTOBUF(p));
      }
      if (((uptr)tf) & PORT_FLAG_INPUT)
      {
        shadow_pointer(PORTIBUF(p));
      }
      shadow_pointer(PORTINFO(p));
      shadow_pointer(PORTNAME(p));
    }
    else if (TYPEP(tf, mask_code, type_code))
    {
      shadow_pointer(CODENAME(p));
      shadow_pointer(CODEARITYMASK(p));
      shadow_pointer(CODEINFO(p));
      shadow_pointer(CODEPINFOS(p));
      {
        ptr t = CODERELOC(p);
        {
          iptr m = (t
                    ? (RELOCSIZE(t))
                    : 0);
          {
            ptr oldco = (t
                         ? (RELOCCODE(t))
                         : 0);
            {
              iptr a = 0;
              {
                iptr n = 0;
                while (n < m)
                {
                  {
                    uptr entry = RELOCIT(t, n);
                    uptr item_off = 0;
                    uptr code_off = 0;
                    n = n + 1;
                    if (RELOC_EXTENDED_FORMAT(entry))
                    {
                      item_off = RELOCIT(t, n);
                      n = n + 1;
                      code_off = RELOCIT(t, n);
                      n = n + 1;
                    }
                    else
                    {
                      item_off = RELOC_ITEM_OFFSET(entry);
                      code_off = RELOC_CODE_OFFSET(entry);
                    }
                    a = a + code_off;
                    {
                      ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                      shadow_pointer(obj);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_thread)
    {
      {
        ptr tc = (ptr)(THREADTC(p));
        if (tc != ((ptr)0))
        {
          STACKCACHE(tc) = Snil;
          shadow_pointer(CCHAIN(tc));
          shadow_pointer(STACKLINK(tc));
          shadow_pointer(WINDERS(tc));
          shadow_pointer(ATTACHMENTS(tc));
          shadow_pointer(HANDLERSTACK(tc));
          shadow_pointer(CURRENTNATIVEFIBER(tc));
          shadow_pointer(FIBERSWITCHPROHIBITEDDEPTH(tc));
          shadow_pointer(NATIVEFIBERCLAIMED(tc));
          shadow_pointer(NATIVEFIBERCLAIMEDCONTROL(tc));
          shadow_pointer(NATIVEFIBERPINNEDHEAD(tc));
          shadow_pointer(NATIVEFIBERTESTACTIVE(tc));
          shadow_pointer(NATIVEFIBERPREEMPTTARGET(tc));
          shadow_pointer(NATIVEFIBERPREEMPTPAYLOAD(tc));
          {
            ptr xcp = FRAME(tc, 0);
            {
              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
              {
                ptr c_p = (ptr)(((uptr)xcp) - co);
                shadow_pointer(c_p);
                {
                  uptr base = (uptr)(SCHEMESTACK(tc));
                  {
                    uptr fp = (uptr)(SFP(tc));
                    {
                      uptr ret = (uptr)(FRAME(tc, 0));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                shadow_pointer(c_p);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        shadow_pointer(*(pp));
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  shadow_pointer(num);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            shadow_pointer(*(pp));
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      shadow_pointer(THREADNO(tc));
                      shadow_pointer(CURRENTINPUT(tc));
                      shadow_pointer(CURRENTOUTPUT(tc));
                      shadow_pointer(CURRENTERROR(tc));
                      shadow_pointer(SFD(tc));
                      shadow_pointer(CURRENTMSO(tc));
                      shadow_pointer(TARGETMACHINE(tc));
                      shadow_pointer(FXLENGTHBV(tc));
                      shadow_pointer(FXFIRSTBITSETBV(tc));
                      shadow_pointer(COMPILEPROFILE(tc));
                      shadow_pointer(SUBSETMODE(tc));
                      shadow_pointer(DEFAULTRECORDEQUALPROCEDURE(tc));
                      shadow_pointer(DEFAULTRECORDHASHPROCEDURE(tc));
                      shadow_pointer(COMPRESSFORMAT(tc));
                      shadow_pointer(COMPRESSLEVEL(tc));
                      shadow_pointer(PARAMETERS(tc));
                      {
                        INT i = 0;
                        while (i < virtual_register_count)
                        {
                          shadow_pointer(VIRTREG(tc, i));
                          i += 1;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_rtd_counts)
    {
    }
    else if ((iptr)tf == type_phantom)
    {
    }
    else
    {
      S_error_abort("shadow: illegal typed object type");
    }
  }
  else if (t == type_pair)
  {
    {
      ISPC p_at_spc = SPACE(p);
      if (p_at_spc < space_weakpair)
      {
        shadow_pointer(INITCAR(p));
        shadow_pointer(INITCDR(p));
      }
      else if (p_at_spc == space_ephemeron)
      {
        shadow_pointer(INITCAR(p));
        shadow_pointer(INITCDR(p));
      }
      else if (p_at_spc == space_weakpair)
      {
        shadow_pointer(INITCDR(p));
      }
      else
      {
        S_error_abort("misplaced pair");
      }
    }
  }
  else if (t == type_closure)
  {
    ptr code = CLOSCODE(p);
    shadow_pointer(code);
    if ((GC_CODE_TYPE(code)) & (code_flag_continuation << code_flags_offset))
    {
      shadow_pointer(CONTWINDERS(p));
      shadow_pointer(CONTATTACHMENTS(p));
      if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
      {
      }
      else
      {
        shadow_pointer(CONTLINK(p));
        {
          ptr xcp = CONTRET(p);
          {
            iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
            {
              ptr c_p = (ptr)(((uptr)xcp) - co);
              shadow_pointer(c_p);
              {
                uptr stack = (uptr)(CONTSTACK(p));
                {
                  uptr base = stack;
                  {
                    uptr fp = stack + (CONTCLENGTH(p));
                    {
                      uptr ret = (uptr)(CONTRET(p));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                shadow_pointer(c_p);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        shadow_pointer(*(pp));
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  shadow_pointer(num);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            shadow_pointer(*(pp));
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else
    {
      uptr len = CODEFREE(code);
      {
        uptr idx, p_len = len;
        ptr *p_p = &CLOSIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_pointer(p_p[idx]);
        }
      }
    }
  }
  else if (t == type_symbol)
  {
    shadow_pointer(INITSYMVAL(p));
    {
      ptr val = INITSYMVAL(p);
      {
        ptr code = ((Sprocedurep(val))
                    ? (CLOSCODE(val))
                    : (SYMCODE(p)));
        shadow_pointer(code);
        shadow_pointer(INITSYMPLIST(p));
        shadow_pointer(INITSYMNAME(p));
        shadow_pointer(INITSYMSPLIST(p));
        shadow_pointer(INITSYMHASH(p));
      }
    }
  }
  else if (t == type_flonum)
  {
  }
  else
  {
    S_error_abort("shadow: illegal type");
  }
}

static void checkmark_object(ptr p)
{
  ITYPE t = TYPEBITS(p);
  if (t == type_typed_object)
  {
    ptr tf = TYPEFIELD(p);
    if (TYPEP(tf, mask_record, type_record))
    {
      shadow_expected_pointer(RECORDINSTTYPE(p));
      {
        ptr rtd = RECORDINSTTYPE(p);
        {
          uptr len = UNFIX((RECORDDESCSIZE(rtd)));
          {
            ptr num = RECORDDESCPM(rtd);
            ptr* pp = &(RECORDINSTIT(p, 0));
            if (Sfixnump(num))
            {
              {
                uptr mask = ((uptr)(UNFIX(num))) >> 1;
                if (mask == (((uptr)-1) >> 1))
                {
                  {
                    ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                    while (pp < ppend)
                    {
                      shadow_expected_pointer(*(pp));
                      pp += 1;
                    }
                  }
                }
                else
                {
                  while (mask != 0)
                  {
                    if (mask & 1)
                    {
                      shadow_expected_pointer(*(pp));
                    }
                    mask >>= 1;
                    pp += 1;
                  }
                }
              }
            }
            else if (num == Strue)
            {
              uptr offset = (uptr)(*((pp + 1)));
              {
                ptr obj = TO_PTR((((uptr)(*(pp))) - offset));
                shadow_expected_pointer(obj);
                *(pp) = TO_PTR((((uptr)obj) + offset));
              }
            }
            else
            {
              shadow_expected_pointer(num);
              check_bignum(num);
              {
                iptr index = (BIGLEN(num)) - 1;
                bigit mask = (BIGIT(num, index)) >> 1;
                INT bits = bigit_bits - 1;
                while (1)
                {
                  do
                  {
                    if (mask & 1)
                    {
                      shadow_expected_pointer(*(pp));
                    }
                    mask >>= 1;
                    pp += 1;
                    bits -= 1;
                  }
                  while (bits > 0);
                  if (index == 0)
                  {
                    break;
                  }
                  index -= 1;
                  mask = BIGIT(num, index);
                  bits = bigit_bits;
                }
              }
            }
          }
        }
      }
    }
    else if (TYPEP(tf, mask_vector, type_vector))
    {
      uptr len = Svector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_expected_pointer(p_p[idx]);
        }
      }
    }
    else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
    {
      uptr len = Sstencil_vector_length(p);
      {
        uptr idx, p_len = len;
        ptr *p_p = &INITSTENVECTIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_expected_pointer(p_p[idx]);
        }
      }
    }
    else if (TYPEP(tf, mask_string, type_string))
    {
    }
    else if (TYPEP(tf, mask_fxvector, type_fxvector))
    {
    }
    else if (TYPEP(tf, mask_flvector, type_flvector))
    {
    }
    else if (TYPEP(tf, mask_bytevector, type_bytevector))
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_reference_array)
        {
          {
            uptr len = Sbytevector_reference_length(p);
            {
              uptr idx, p_len = len;
              ptr *p_p = (ptr*)&BVIT(p, 0);
              for (idx = 0; idx < p_len; idx++)
              {
                shadow_expected_pointer(S_maybe_reference_to_object(p_p[idx]));
              }
            }
          }
        }
        else
        {
        }
      }
    }
    else if ((iptr)tf == type_tlc)
    {
      shadow_expected_pointer(INITTLCHT(p));
      shadow_expected_pointer(INITTLCKEYVAL(p));
      shadow_expected_pointer(INITTLCNEXT(p));
    }
    else if (TYPEP(tf, mask_box, type_box))
    {
      shadow_expected_pointer(INITBOXREF(p));
    }
    else if ((iptr)tf == type_ratnum)
    {
      shadow_expected_pointer(RATNUM(p));
      shadow_expected_pointer(RATDEN(p));
    }
    else if ((iptr)tf == type_exactnum)
    {
      shadow_expected_pointer(EXACTNUM_REAL_PART(p));
      shadow_expected_pointer(EXACTNUM_IMAG_PART(p));
    }
    else if ((iptr)tf == type_inexactnum)
    {
    }
    else if (TYPEP(tf, mask_bignum, type_bignum))
    {
    }
    else if (TYPEP(tf, mask_port, type_port))
    {
      shadow_expected_pointer(PORTHANDLER(p));
      if (((uptr)tf) & PORT_FLAG_OUTPUT)
      {
        shadow_expected_pointer(PORTOBUF(p));
      }
      if (((uptr)tf) & PORT_FLAG_INPUT)
      {
        shadow_expected_pointer(PORTIBUF(p));
      }
      shadow_expected_pointer(PORTINFO(p));
      shadow_expected_pointer(PORTNAME(p));
    }
    else if (TYPEP(tf, mask_code, type_code))
    {
      shadow_expected_pointer(CODENAME(p));
      shadow_expected_pointer(CODEARITYMASK(p));
      shadow_expected_pointer(CODEINFO(p));
      shadow_expected_pointer(CODEPINFOS(p));
      {
        ptr t = CODERELOC(p);
        {
          iptr m = (t
                    ? (RELOCSIZE(t))
                    : 0);
          {
            ptr oldco = (t
                         ? (RELOCCODE(t))
                         : 0);
            {
              iptr a = 0;
              {
                iptr n = 0;
                while (n < m)
                {
                  {
                    uptr entry = RELOCIT(t, n);
                    uptr item_off = 0;
                    uptr code_off = 0;
                    n = n + 1;
                    if (RELOC_EXTENDED_FORMAT(entry))
                    {
                      item_off = RELOCIT(t, n);
                      n = n + 1;
                      code_off = RELOCIT(t, n);
                      n = n + 1;
                    }
                    else
                    {
                      item_off = RELOC_ITEM_OFFSET(entry);
                      code_off = RELOC_CODE_OFFSET(entry);
                    }
                    a = a + code_off;
                    {
                      ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                      shadow_expected_pointer(obj);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_thread)
    {
      {
        ptr tc = (ptr)(THREADTC(p));
        if (tc != ((ptr)0))
        {
          STACKCACHE(tc) = Snil;
          shadow_expected_pointer(CCHAIN(tc));
          shadow_expected_pointer(STACKLINK(tc));
          shadow_expected_pointer(WINDERS(tc));
          shadow_expected_pointer(ATTACHMENTS(tc));
          shadow_expected_pointer(HANDLERSTACK(tc));
          shadow_expected_pointer(CURRENTNATIVEFIBER(tc));
          shadow_expected_pointer(FIBERSWITCHPROHIBITEDDEPTH(tc));
          shadow_expected_pointer(NATIVEFIBERCLAIMED(tc));
          shadow_expected_pointer(NATIVEFIBERCLAIMEDCONTROL(tc));
          shadow_expected_pointer(NATIVEFIBERPINNEDHEAD(tc));
          shadow_expected_pointer(NATIVEFIBERTESTACTIVE(tc));
          shadow_expected_pointer(NATIVEFIBERPREEMPTTARGET(tc));
          shadow_expected_pointer(NATIVEFIBERPREEMPTPAYLOAD(tc));
          {
            ptr xcp = FRAME(tc, 0);
            {
              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
              {
                ptr c_p = (ptr)(((uptr)xcp) - co);
                shadow_expected_pointer(c_p);
                {
                  uptr base = (uptr)(SCHEMESTACK(tc));
                  {
                    uptr fp = (uptr)(SFP(tc));
                    {
                      uptr ret = (uptr)(FRAME(tc, 0));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                shadow_expected_pointer(c_p);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        shadow_expected_pointer(*(pp));
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  shadow_expected_pointer(num);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            shadow_expected_pointer(*(pp));
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      shadow_expected_pointer(THREADNO(tc));
                      shadow_expected_pointer(CURRENTINPUT(tc));
                      shadow_expected_pointer(CURRENTOUTPUT(tc));
                      shadow_expected_pointer(CURRENTERROR(tc));
                      shadow_expected_pointer(SFD(tc));
                      shadow_expected_pointer(CURRENTMSO(tc));
                      shadow_expected_pointer(TARGETMACHINE(tc));
                      shadow_expected_pointer(FXLENGTHBV(tc));
                      shadow_expected_pointer(FXFIRSTBITSETBV(tc));
                      shadow_expected_pointer(COMPILEPROFILE(tc));
                      shadow_expected_pointer(SUBSETMODE(tc));
                      shadow_expected_pointer(DEFAULTRECORDEQUALPROCEDURE(tc));
                      shadow_expected_pointer(DEFAULTRECORDHASHPROCEDURE(tc));
                      shadow_expected_pointer(COMPRESSFORMAT(tc));
                      shadow_expected_pointer(COMPRESSLEVEL(tc));
                      shadow_expected_pointer(PARAMETERS(tc));
                      {
                        INT i = 0;
                        while (i < virtual_register_count)
                        {
                          shadow_expected_pointer(VIRTREG(tc, i));
                          i += 1;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else if ((iptr)tf == type_rtd_counts)
    {
    }
    else if ((iptr)tf == type_phantom)
    {
    }
    else
    {
      S_error_abort("checkmark: illegal typed object type");
    }
  }
  else if (t == type_pair)
  {
    {
      ISPC p_at_spc = SPACE(p);
      if (p_at_spc < space_weakpair)
      {
        shadow_expected_pointer(INITCAR(p));
        shadow_expected_pointer(INITCDR(p));
      }
      else if (p_at_spc == space_ephemeron)
      {
        shadow_ephemeron(p);
      }
      else if (p_at_spc == space_weakpair)
      {
        shadow_weak_pair(p);
        shadow_expected_pointer(INITCDR(p));
      }
      else
      {
        S_error_abort("misplaced pair");
      }
    }
  }
  else if (t == type_closure)
  {
    ptr code = CLOSCODE(p);
    shadow_expected_pointer(code);
    if ((GC_CODE_TYPE(code)) & (code_flag_continuation << code_flags_offset))
    {
      shadow_expected_pointer(CONTWINDERS(p));
      shadow_expected_pointer(CONTATTACHMENTS(p));
      if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
      {
      }
      else
      {
        shadow_expected_pointer(CONTLINK(p));
        {
          ptr xcp = CONTRET(p);
          {
            iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
            {
              ptr c_p = (ptr)(((uptr)xcp) - co);
              shadow_expected_pointer(c_p);
              {
                uptr stack = (uptr)(CONTSTACK(p));
                {
                  uptr base = stack;
                  {
                    uptr fp = stack + (CONTCLENGTH(p));
                    {
                      uptr ret = (uptr)(CONTRET(p));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                shadow_expected_pointer(c_p);
                                if (Sfixnump(num))
                                {
                                  {
                                    uptr mask = UNFIX(num);
                                    while (mask != 0)
                                    {
                                      pp += 1;
                                      if (mask & 1)
                                      {
                                        shadow_expected_pointer(*(pp));
                                      }
                                      mask >>= 1;
                                    }
                                  }
                                }
                                else
                                {
                                  shadow_expected_pointer(num);
                                  check_bignum(num);
                                  {
                                    iptr index = BIGLEN(num);
                                    while (index != 0)
                                    {
                                      index -= 1;
                                      {
                                        INT bits = bigit_bits;
                                        bigit mask = BIGIT(num, index);
                                        while (bits > 0)
                                        {
                                          bits -= 1;
                                          pp += 1;
                                          if (mask & 1)
                                          {
                                            shadow_expected_pointer(*(pp));
                                          }
                                          mask >>= 1;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    else
    {
      uptr len = CODEFREE(code);
      {
        uptr idx, p_len = len;
        ptr *p_p = &CLOSIT(p, 0);
        for (idx = 0; idx < p_len; idx++)
        {
          shadow_expected_pointer(p_p[idx]);
        }
      }
    }
  }
  else if (t == type_symbol)
  {
    shadow_expected_pointer(INITSYMVAL(p));
    {
      ptr val = INITSYMVAL(p);
      {
        ptr code = ((Sprocedurep(val))
                    ? (CLOSCODE(val))
                    : (SYMCODE(p)));
        shadow_expected_pointer(code);
        shadow_expected_pointer(INITSYMPLIST(p));
        shadow_expected_pointer(INITSYMNAME(p));
        shadow_expected_pointer(INITSYMSPLIST(p));
        shadow_expected_pointer(INITSYMHASH(p));
      }
    }
  }
  else if (t == type_flonum)
  {
  }
  else
  {
    S_error_abort("checkmark: illegal type");
  }
}

static uptr size_object(ptr p)
{
  ITYPE t = TYPEBITS(p);
  if (t == type_typed_object)
  {
    ptr tf = TYPEFIELD(p);
    if (TYPEP(tf, mask_record, type_record))
    {
      {
        ptr rtd = RECORDINSTTYPE(p);
        {
          uptr len = UNFIX((RECORDDESCSIZE(rtd)));
          {
            uptr p_sz = size_record_inst(len);
            return p_sz;
          }
        }
      }
    }
    else if (TYPEP(tf, mask_vector, type_vector))
    {
      uptr len = Svector_length(p);
      {
        uptr p_sz = size_vector(len);
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
    {
      uptr len = Sstencil_vector_length(p);
      {
        uptr p_sz = size_stencil_vector(len);
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_string, type_string))
    {
      uptr sz = size_string((Sstring_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_fxvector, type_fxvector))
    {
      uptr sz = size_fxvector((Sfxvector_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_flvector, type_flvector))
    {
      uptr sz = size_flvector((Sflvector_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_bytevector, type_bytevector))
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_reference_array)
        {
          uptr sz = size_bytevector((Sbytevector_length(p)));
          {
            uptr p_sz = sz;
            return p_sz;
          }
        }
        else
        {
          uptr sz = size_bytevector((Sbytevector_length(p)));
          {
            uptr p_sz = sz;
            return p_sz;
          }
        }
      }
    }
    else if ((iptr)tf == type_tlc)
    {
      uptr p_sz = size_tlc;
      return p_sz;
    }
    else if (TYPEP(tf, mask_box, type_box))
    {
      uptr p_sz = size_box;
      return p_sz;
    }
    else if ((iptr)tf == type_ratnum)
    {
      uptr p_sz = size_ratnum;
      return p_sz;
    }
    else if ((iptr)tf == type_exactnum)
    {
      uptr p_sz = size_exactnum;
      return p_sz;
    }
    else if ((iptr)tf == type_inexactnum)
    {
      uptr p_sz = size_inexactnum;
      return p_sz;
    }
    else if (TYPEP(tf, mask_bignum, type_bignum))
    {
      uptr sz = size_bignum((BIGLEN(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_port, type_port))
    {
      uptr p_sz = size_port;
      return p_sz;
    }
    else if (TYPEP(tf, mask_code, type_code))
    {
      uptr len = CODELEN(p);
      {
        uptr p_sz = size_code(len);
        return p_sz;
      }
    }
    else if ((iptr)tf == type_thread)
    {
      uptr p_sz = size_thread;
      return p_sz;
    }
    else if ((iptr)tf == type_rtd_counts)
    {
      uptr p_sz = size_rtd_counts;
      return p_sz;
    }
    else if ((iptr)tf == type_phantom)
    {
      uptr p_sz = size_phantom;
      return p_sz;
    }
    else
    {
      S_error_abort("size: illegal typed object type");
    }
  }
  else if (t == type_pair)
  {
    {
      ISPC p_at_spc = SPACE(p);
      if (p_at_spc < space_weakpair)
      {
        uptr p_sz = size_pair;
        return p_sz;
      }
      else if (p_at_spc == space_ephemeron)
      {
        uptr p_sz = size_ephemeron;
        return p_sz;
      }
      else if (p_at_spc == space_weakpair)
      {
        uptr p_sz = size_pair;
        return p_sz;
      }
      else
      {
        S_error_abort("misplaced pair");
      }
    }
  }
  else if (t == type_closure)
  {
    ptr code = CLOSCODE(p);
    if ((GC_CODE_TYPE(code)) & (code_flag_continuation << code_flags_offset))
    {
      uptr p_sz = size_continuation;
      return p_sz;
    }
    else
    {
      uptr len = CODEFREE(code);
      {
        uptr p_sz = size_closure(len);
        return p_sz;
      }
    }
  }
  else if (t == type_symbol)
  {
    uptr p_sz = size_symbol;
    return p_sz;
  }
  else if (t == type_flonum)
  {
    uptr p_sz = size_flonum;
    return p_sz;
  }
  else
  {
    S_error_abort("size: illegal type");
  }
}

