#include <R.h>
#include <Rinternals.h>

SEXP makeOutputList(SEXP vertices, unsigned nvertices, SEXP faces, unsigned nfaces){
  SEXP out, names, dimvs, dimfs;
  unsigned nprotect = 0;

  PROTECT(dimvs = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dimvs)[0] = 3; INTEGER(dimvs)[1] = nvertices;
  setAttrib(vertices, R_DimSymbol, dimvs);

  PROTECT(dimfs = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dimfs)[0] = 3; INTEGER(dimfs)[1] = nfaces;
  setAttrib(faces, R_DimSymbol, dimfs);

  PROTECT(out = allocVector(VECSXP, 2));
  nprotect++;
  SET_VECTOR_ELT(out, 0, vertices);
  SET_VECTOR_ELT(out, 1, faces);

  PROTECT(names = allocVector(VECSXP, 2));
  nprotect++;
  SET_VECTOR_ELT(names, 0, mkChar("vertices"));
  SET_VECTOR_ELT(names, 1, mkChar("faces"));
  setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(nprotect);
  return out;
}
