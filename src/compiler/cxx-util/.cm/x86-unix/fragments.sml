110.81  x86    
            @       j      C�  U�5;�0erho�f�V      �%��$JB���`����%��$JB���`���               n               n�5;�0erho�f�guid-driver/(sources.cm):../target-cpu/(sources.cm):../cxx-util/(sources.cm):fragments.sml-1510777893.861
  A�    �"      �/*---------- begin cxx-head.in ----------*/
/*! \file @CXXFILE@
 *
 * Generated from @SRCFILE@.
 *
 * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@
 * Version: @DIDEROTC_VERSION@
 */
/*---------- end cxx-head.in ----------*/
  �/*---------- begin debugger-incl.in ----------*/
#define DIDEROT_STANDALONE_EXEC
#define @DIDEROT_FLOAT_PRECISION@
#define @DIDEROT_INT_PRECISION@
#define @DIDEROT_TARGET@
#include "diderot/diderot.hxx"

#ifdef DIDEROT_ENABLE_LOGGING
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

static std::string ProgramName = "@PROG_NAME@";

extern "C" {
typedef struct @PREFIX@_struct_world @PREFIX@_world_t;
}
/*---------- end debugger-incl.in ----------*/
  /*---------- begin eigenvals2x2.in ----------*/
static int eigenvals (tensor_ref_2_2 mat, diderot::array<@REALTY@,2> &eval)
{
    int roots;

  /* copy the given matrix elements */
    @REALTY@ M00 = mat[0];
    @REALTY@ M01 = mat[1];
    @REALTY@ M11 = mat[3];

  /* subtract out the eigenvalue mean (we will add it back to evals later);
   * helps with numerical stability
   */
    @REALTY@ mean = @REALTY@(0.5) * (M00 + M11);
    M00 -= mean;
    M11 -= mean;

    @REALTY@ Q = M00 - M11;
    @REALTY@ D = @REALTY@(4)*M01*M01 + Q*Q;
    if (D > diderot::__details::EPSILON) {
      /* two distinct roots */
        @REALTY@ vv = @REALTY@(0.5) * std::sqrt(D);
        eval[0] = vv;
        eval[1] = -vv;
        roots = diderot::__details::ROOT_TWO;
    }
    else {
      /* double root */
        eval[0] = eval[1] = @REALTY@(0);
        roots = diderot::__details::ROOT_DOUBLE;
    }

  /* add back in the eigenvalue mean */
    eval[0] += mean;
    eval[1] += mean;

    return roots;
}
/*---------- end eigenvals2x2.in ----------*/
  �/*---------- begin eigenvecs2x2.in ----------*/
static int eigenvecs (tensor_ref_2_2 mat, diderot::array<@REALTY@,2> &eval, diderot::array<tensor_2,2> &evec)
{
    int roots;

  /* copy the given matrix elements */
    @REALTY@ M00 = mat[0];
    @REALTY@ M01 = mat[1];
    @REALTY@ M11 = mat[3];

  /* subtract out the eigenvalue mean (we will add it back to evals later);
   * helps with numerical stability
   */
    @REALTY@ mean = @REALTY@(0.5) * (M00 + M11);
    M00 -= mean;
    M11 -= mean;

    @REALTY@ Q = M00 - M11;
    @REALTY@ D = @REALTY@(4)*M01*M01 + Q*Q;
    if (D > diderot::__details::EPSILON) {
      /* two distinct roots */
        @REALTY@ vv = @REALTY@(0.5) * std::sqrt(D);
        eval[0] = vv;
        eval[1] = -vv;
      /* null space of T = M - evec[0]*I ==
         [M00 - vv      M01  ]
         [  M01      M11 - vv]
         is evec[0], but we know evec[0] and evec[1] are orthogonal,
         so row span of T is evec[1]
      */
        @REALTY@ r1[2] = { M00 - vv, M01 };
        @REALTY@ r2[2] = { M01, M11 - vv };
        if ((r1[0]*r2[0] + r1[1]*r2[1]) > @REALTY@(0)) {
            evec[1][0] = r1[0] + r2[0];
            evec[1][1] = r1[1] + r2[1];
        }
        else {
            evec[1][0] = r1[0] - r2[0];
            evec[1][1] = r1[1] - r2[1];
        }
        diderot::__details::normalize2 (evec[1]._data);
        evec[0][0] = evec[1][1];
        evec[0][1] = -evec[1][0];
        diderot::__details::normalize2 (evec[0]._data);
        roots = diderot::__details::ROOT_TWO;
    }
    else {
      /* double root */
        eval[0] = eval[1] = @REALTY@(0.0);
      /* use any basis for eigenvectors */
        evec[0][0] = @REALTY@(1.0);
        evec[0][1] = @REALTY@(0.0);
        evec[1][0] = @REALTY@(0.0);
        evec[1][1] = @REALTY@(1.0);
        roots = diderot::__details::ROOT_DOUBLE;
    }

    /* add back in the eigenvalue mean */
    eval[0] += mean;
    eval[1] += mean;

    return roots;
}
/*---------- end eigenvecs2x2.in ----------*/
  ,/*---------- begin eigenvals3x3.in ----------*/
// from http://en.wikipedia.org/wiki/Square_root_of_3
#define M_SQRT3 @REALTY@(1.732050807568877293527446341506)

static int eigenvals (tensor_ref_3_3 const &mat, diderot::array<@REALTY@,3> &eval)
{
    int roots;

  /* copy the given matrix elements */
    @REALTY@ M00 = mat[0];
    @REALTY@ M01 = mat[1];
    @REALTY@ M02 = mat[2];
    @REALTY@ M11 = mat[4];
    @REALTY@ M12 = mat[5];
    @REALTY@ M22 = mat[8];

  /* subtract out the eigenvalue mean (we will add it back to evals later);
   * helps with numerical stability
   */
    @REALTY@ mean = (M00 + M11 + M22) / @REALTY@(3);
    M00 -= mean;
    M11 -= mean;
    M22 -= mean;

  /*
  ** divide out L2 norm of eigenvalues (will multiply back later);
  ** this too seems to help with stability
  */
    @REALTY@ norm = std::sqrt(M00*M00 + 2*M01*M01 + 2*M02*M02 + M11*M11 + 2*M12*M12 + M22*M22);
    @REALTY@ rnorm = (norm > diderot::__details::EPSILON) ? @REALTY@(1) / norm : @REALTY@(1);
    M00 *= rnorm;
    M01 *= rnorm;
    M02 *= rnorm;
    M11 *= rnorm;
    M12 *= rnorm;
    M22 *= rnorm;

  /* this code is a mix of prior Teem code and ideas from Eberly's
   * "Eigensystems for 3 x 3 Symmetric Matrices (Revisited)"
   */
    @REALTY@ Q = (M01*M01 + M02*M02 + M12*M12 - M00*M11 - M00*M22 - M11*M22) / @REALTY@(3);
    @REALTY@ QQQ = Q*Q*Q;
    @REALTY@ R = @REALTY@(0.5) * (M00*M11*M22 + M02*(2*M01*M12 - M02*M11) - M00*M12*M12 - M01*M01*M22);
    @REALTY@ D = QQQ - R*R;
    if (D > diderot::__details::EPSILON) {
      /* three distinct roots- this is the most common case */
        @REALTY@ theta = std::atan2(std::sqrt(D), R) / @REALTY@(3);
        @REALTY@ mm = std::sqrt(Q);
        @REALTY@ ss = std::sin(theta);
        @REALTY@ cc = std::cos(theta);
        eval[0] = 2*mm*cc;
        eval[1] = mm*(-cc + M_SQRT3 * ss);
        eval[2] = mm*(-cc - M_SQRT3 * ss);
        roots = diderot::__details::ROOT_THREE;
    }
  /* else D is near enough to zero */
    else if (std::abs(R) > diderot::__details::EPSILON) {
      /* one double root and one single root */
        @REALTY@ U = std::cbrt(R); /* cube root function */
        if (U > 0) {
            eval[0] = 2*U;
            eval[1] = -U;
            eval[2] = -U;
        }
        else {
            eval[0] = -U;
            eval[1] = -U;
            eval[2] = 2*U;
        }
        roots = diderot::__details::ROOT_SINGLE_DOUBLE;
    }
    else {
      /* a triple root! */
        eval[0] = eval[1] = eval[2] = 0.0;
        roots = diderot::__details::ROOT_TRIPLE;
    }

  /* multiply back by eigenvalue L2 norm */
    eval[0] /= rnorm;
    eval[1] /= rnorm;
    eval[2] /= rnorm;

  /* add back in the eigenvalue mean */
    eval[0] += mean;
    eval[1] += mean;
    eval[2] += mean;

    return roots;
}

#undef M_SQRT3
/*---------- end eigenvals3x3.in ----------*/
  ~/*---------- begin eigenvecs3x3.in ----------*/
inline @REALTY@ dot3 (tensor_ref_3 const & a, tensor_ref_3 const & b)
{
    return (a[0]*b[0] + a[1]*b[1] + a[2]*b[2]);
}

inline void cross3 (tensor_ref_3 const & v1, tensor_ref_3 const & v2, tensor_3 & res)
{
    res[0] = v1[1]*v2[2] - v1[2]*v2[1];
    res[1] = v1[2]*v2[0] - v1[0]*v2[2];
    res[2] = v1[0]*v2[1] - v1[1]*v2[0];
}

inline void nullspace1 (
    tensor_ref_3 const & r0,
    tensor_ref_3 const & r1,
    tensor_ref_3 const & r2,
    tensor_3 & res)
{
    tensor_3 crs;

    cross3(r0, r1, res);
    cross3(r1, r2, crs);

  /* ret += crs or ret -= crs; whichever makes res longer */
    if (dot3(res, crs) > 0.0) {
	res[0] += crs[0];
	res[1] += crs[1];
	res[2] += crs[2];
    } else {
	res[0] -= crs[0];
	res[1] -= crs[1];
	res[2] -= crs[2];
    }

    cross3(r0, r2, crs);
  /* ret += crs or ret -= crs; whichever makes res longer */
    if (dot3(res, crs) > 0.0) {
	res[0] += crs[0];
	res[1] += crs[1];
	res[2] += crs[2];
    } else {
	res[0] -= crs[0];
	res[1] -= crs[1];
	res[2] -= crs[2];
    }
}

/*
** All vectors are in the same 1D space, we have to find two
** mutually vectors perpendicular to that span
*/
static void nullspace2 (
    tensor_ref_3 const & r0,
    tensor_ref_3 const & r1,
    tensor_ref_3 const & r2,
    tensor_3 *rets)  // will point to either evec[0] or evec[1]
{
    tensor_3 sqr, sum;
    int idx;

    sum = r0;
    if (dot3(sum, r1) > 0) {
	sum[0] += r1[0];
	sum[1] += r1[1];
	sum[2] += r1[2];
    } else {
	sum[0] -= r1[0];
	sum[1] -= r1[1];
	sum[2] -= r1[2];
    }
    if (dot3(sum, r2) > 0) {
	sum[0] += r2[0];
	sum[1] += r2[1];
	sum[2] += r2[2];
    } else {
	sum[0] -= r2[0];
	sum[1] -= r2[1];
	sum[2] -= r2[2];
    }
  // find largest component, to get most stable expression for a perpendicular vector
    sqr[0] = sum[0]*sum[0];
    sqr[1] = sum[1]*sum[1];
    sqr[2] = sum[2]*sum[2];
    idx = 0;
    if (sqr[0] < sqr[1]) {
	idx = 1;
    }
    if (sqr[idx] < sqr[2]) {
	idx = 2;
    }

    if (0 == idx) {
      rets[0] = {sum[1] - sum[2], -sum[0], sum[0]};
    } else if (1 == idx) {
      rets[0] = {-sum[1], sum[0] - sum[2], sum[1]};
    } else {
      rets[0] = {-sum[2], sum[2], sum[0] - sum[1]};
    }

    cross3(rets[0], sum, rets[1]);
    return;
}

static int eigenvecs (tensor_ref_3_3 const &mat, diderot::array<@REALTY@, 3> &eval, diderot::array<tensor_3,3> &evec)
{
    @REALTY@ len, dot;
    int roots;

  /* copy the given matrix elements */
    @REALTY@ M00 = mat[0];
    @REALTY@ M01 = mat[1];
    @REALTY@ M02 = mat[2];
    @REALTY@ M11 = mat[4];
    @REALTY@ M12 = mat[5];
    @REALTY@ M22 = mat[8];

  /*
  ** subtract out the eigenvalue mean (will add back to evals later);
  ** helps with numerical stability
  */
    @REALTY@ mean = (M00 + M11 + M22) / @REALTY@(3);
    M00 -= mean;
    M11 -= mean;
    M22 -= mean;

  /*
  ** divide out L2 norm of eigenvalues (will multiply back later);
  ** this too seems to help with stability
  */
    @REALTY@ norm = std::sqrt(M00*M00 + 2*M01*M01 + 2*M02*M02 + M11*M11 + 2*M12*M12 + M22*M22);
    @REALTY@ rnorm = (norm > diderot::__details::EPSILON) ? @REALTY@(1) / norm : @REALTY@(1);
    M00 *= rnorm;
    M01 *= rnorm;
    M02 *= rnorm;
    M11 *= rnorm;
    M12 *= rnorm;
    M22 *= rnorm;

  /* this code is a mix of prior Teem code and ideas from Eberly's
   * "Eigensystems for 3 x 3 Symmetric Matrices (Revisited)"
   */
    @REALTY@ Q = (M01*M01 + M02*M02 + M12*M12 - M00*M11 - M00*M22 - M11*M22)/@REALTY@(3);
    @REALTY@ QQQ = Q*Q*Q;
    @REALTY@ R = @REALTY@(0.5)*(M00*M11*M22 + M02*(2*M01*M12 - M02*M11) - M00*M12*M12 - M01*M01*M22);
    @REALTY@ D = QQQ - R*R;
    if (D > diderot::__details::EPSILON) {
      /* three distinct roots- this is the most common case */
        @REALTY@ theta = std::atan2(std::sqrt(D), R) / @REALTY@(3);
        @REALTY@ mm = std::sqrt(Q);
        @REALTY@ ss = std::sin(theta);
        @REALTY@ cc = std::cos(theta);
        eval[0] = 2*mm*cc;
        eval[1] = mm*(-cc + std::sqrt(3.0)*ss);
        eval[2] = mm*(-cc - std::sqrt(3.0)*ss);
        roots = diderot::__details::ROOT_THREE;
    }
  /* else D is near enough to zero */
    else if (std::abs(R) > diderot::__details::EPSILON) {
      /* one double root and one single root */
        @REALTY@ U = std::cbrt(R); /* cube root function */
        if (U > 0) {
            eval[0] = 2*U;
            eval[1] = -U;
            eval[2] = -U;
        } else {
            eval[0] = -U;
            eval[1] = -U;
            eval[2] = 2*U;
        }
        roots = diderot::__details::ROOT_SINGLE_DOUBLE;
    }
    else {
      /* a triple root! */
        eval[0] = eval[1] = eval[2] = 0.0;
        roots = diderot::__details::ROOT_TRIPLE;
    }
/* END #include "teigen-evals-A.c" */

    tensor_3 ev = tensor_3 { eval[0], eval[1], eval[2] };
    if (diderot::__details::ROOT_THREE == roots) {
         nullspace1 (
	    tensor_3 { M00 - eval[0], M01, M02 },
	    tensor_3 { M01, M11 - eval[0], M12 },
	    tensor_3 { M02, M12, M22 - eval[0] },
	    evec[0]);
        nullspace1 (
	    tensor_3 { M00 - eval[1], M01, M02 },
	    tensor_3 { M01, M11 - eval[1], M12 },
	    tensor_3 { M02, M12, M22 - eval[1] },
	    evec[1]);
        nullspace1 (
	    tensor_3 { M00 - eval[2], M01, M02 },
	    tensor_3 { M01, M11 - eval[2], M12 },
	    tensor_3 { M02, M12, M22 - eval[2] },
	    evec[2]);
    }
    else if (diderot::__details::ROOT_SINGLE_DOUBLE == roots) {
        if (eval[1] == eval[2]) {
          /* one big (eval[0]) , two small (eval[1,2]) */
            nullspace1 (
		tensor_3 { M00 - eval[0], M01, M02 },
		tensor_3 { M01, M11 - eval[0], M12 },
		tensor_3 { M02, M12, M22 - eval[0] },
		evec[0]);
            nullspace2 (
		tensor_3 { M00 - eval[1], M01, M02 },
		tensor_3 { M01, M11 - eval[1], M12 },
		tensor_3 { M02, M12, M22 - eval[1] },
		&evec[1]);
        }
        else {
          /* two big (eval[0,1]), one small (eval[2]) */
            nullspace2 (
		tensor_3 { M00 - eval[0], M01, M02 },
		tensor_3 { M01, M11 - eval[0], M12 },
		tensor_3 { M02, M12, M22 - eval[0] },
		&evec[0]);
            nullspace1 (
		tensor_3 { M00 - eval[2], M01, M02 },
		tensor_3 { M01, M11 - eval[2], M12 },
		tensor_3 { M02, M12, M22 - eval[2] },
		evec[2]);
        }
    }
    else {
      /* ROOT_TRIPLE == roots; use any basis for eigenvectors */
        evec[0] = tensor_3 { 1, 0, 0 };
        evec[1] = tensor_3 { 0, 1, 0 };
        evec[2] = tensor_3 { 0, 0, 1 };
    }
  /* we always make sure it's really orthonormal; keeping fixed the
   * eigenvector associated with the largest-magnitude eigenvalue
   */
    if (std::abs(eval[0]) > std::abs(eval[2])) {
      /* normalize evec[0] but don't move it */
        diderot::__details::normalize3(evec[0]._data);
      // compute evec[1] -= scale3(dot3(evec[1], evec[0]), evec[0]);
	@REALTY@ s = dot3(evec[1], evec[0]);
	evec[1][0] -= s*evec[0][0];
	evec[1][1] -= s*evec[0][1];
	evec[1][2] -= s*evec[0][2];
        diderot::__details::normalize3(evec[1]._data);
        cross3(evec[0], evec[1], evec[2]);
    }
    else {
      /* normalize evec[2] but don't move it */
        diderot::__details::normalize3(evec[2]._data);
      // compute evec[1] -= scale3(dot3(evec[1], evec[2]), evec[2]);
	@REALTY@ s = dot3(evec[1], evec[2]);
	evec[1][0] -= s*evec[2][0];
	evec[1][1] -= s*evec[2][1];
	evec[1][2] -= s*evec[2][2];
        diderot::__details::normalize3(evec[1]._data);
        cross3(evec[1], evec[2], evec[0]);
    }
    /* note that the right-handedness check has been folded into
       the code above to enforce orthogonality.  Indeed, some work
       could be removed by never really bothering to find all three
       eigenvectors; just find two and then use the cross-product.
       The next iteration of the code will do that */

  /* multiply back by eigenvalue L2 norm */
    eval[0] /= rnorm;
    eval[1] /= rnorm;
    eval[2] /= rnorm;

  /* add back in the eigenvalue mean */
    eval[0] += mean;
    eval[1] += mean;
    eval[2] += mean;

    return roots;
}
/*---------- end eigenvecs3x3.in ----------*/
  b/*---------- begin exec-incl.in ----------*/
#define DIDEROT_STANDALONE_EXEC
#define @DIDEROT_FLOAT_PRECISION@
#define @DIDEROT_INT_PRECISION@
#define @DIDEROT_TARGET@
#include "diderot/diderot.hxx"

#ifdef DIDEROT_ENABLE_LOGGING
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif
/*---------- end exec-incl.in ----------*/
  2/*---------- begin lib-cxx-incl.in ----------*/
#include "@H_FILE@"
#include "diderot/diderot.hxx"

#ifdef DIDEROT_ENABLE_LOGGING
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

static std::string ProgramName = "@PROG_NAME@";
/*---------- end lib-cxx-incl.in ----------*/
   �/*---------- begin lib-hxx-foot.in ----------*/

} // namespace @PREFIX@

#endif // !@HXX_DEFINE@
/*---------- end lib-hxx-foot.in ----------*/
  �/*---------- begin lib-hxx-head.in ----------*/
/*! \file @HXX_FILE@
 *
 * C++ interface to library generated from @SRCFILE@.
 *
 * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@
 * Version: @DIDEROTC_VERSION@
 */

#ifndef @HXX_DEFINE@
#define @HXX_DEFINE@

#define @DIDEROT_FLOAT_PRECISION@
#define @DIDEROT_INT_PRECISION@
#define @DIDEROT_TARGET@

#include <string>
#include <cstdint>
#include "teem/nrrd.h"

namespace @PREFIX@ {

struct World_t;
/*---------- end lib-hxx-head.in ----------*/
   }/*---------- begin namespace-close.in ----------*/

} // namespace @PREFIX@
/*---------- end namespace-close.in ----------*/
   �/*---------- begin namespace-open.in ----------*/
namespace @PREFIX@ {

static std::string ProgramName = "@PROG_NAME@";

struct world;
struct @STRAND@_strand;
/*---------- end namespace-open.in ----------*/
  �/*---------- begin nrrd-save-helper.in ----------*/
/* helper function for saving output to nrrd file */
inline bool nrrd_save_helper (std::string const &file, Nrrd *nrrd)
{
    if (nrrdSave (file.c_str(), nrrd, nullptr)) {
        std::cerr << "Error saving \"" << file << "\":\n" << biffGetDone(NRRD) << std::endl;
        return true;
    }
    else {
        return false;
    }
}
/*---------- end nrrd-save-helper.in ----------*/
	       	       	   �  �    �D$H� �D$;|$ws�E �D$P��  �E� �G�w�_�O�W�G  �M�O�U�W �]�_$�w�w(�L$P��G�U�M�\$H�\$P�\$L�ؽ   �t$���   ��0�d$H�T  ��x����D$;|$w"�)�l$H�L$L�   �   �t$���   �d$H�  ���@����D$;|$w'�
�L$H�T$L�   �   �   �t$���   �d$H��   ������D$;|$w)�+�u �S�K�[�t$H�l$L�   �D$��<  �d$H�   ���������D$;|$w"�)�l$H�L$L�   �   �t$��p  �d$H�V�������D$;|$w'��L$H�\$L�   �   �   �t$���  �d$H����;|$w�p�(�P�H�X��� �D$H   �D$L   �T$ ���T$ �d$Hcxx-util/fragments.sml 1p�CxxFragments"5DCA nff8pa"cxxHead"4��nC"string"0 pa"debugIncl"4�&pa"eigenvals2x2"4�&Cpa"eigenvecs2x2"4�&pa"eigenvals3x3"4�&pa"eigenvecs3x3"4�&pa"execIncl"4�&pa"libCXXIncl"4�&Cpa"libHXXFoot"4�&pa"libHXXHead"4�&	pa"namespaceClose"4�&
pa"namespaceOpen"4�&pa"nrrdSaveHelper"4�&N00sABi1�A 8�B��,��,C��,��,��,��,��,C��,��,��,��,��,N