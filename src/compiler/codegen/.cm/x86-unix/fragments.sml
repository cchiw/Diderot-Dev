110.81  x86    
            P       i      5�   ��5;�0erho�f�V      ���<�-v��)�w�����<�-v��)�w��               n               n�5;�0erho�f��:�C2`3��/BLN'�guid-driver/(sources.cm):../target-cpu/(sources.cm):../codegen/(sources.cm):fragments.sml-1510777889.846
  /�    �"  �      @PROG_NAME@	      program   	         	@SRCFILE@	      source-file   	         @DIDEROTC_CMD@ @DIDEROTC_ARGV@	      	build-cmd   	         @DIDEROTC_VERSION@	      version   	         @DIDEROT_REAL_SIZE@	      
float-size   	         @DIDEROT_INT_SIZE@	      int-size   	         @DIDEROT_TARGET@	      target   	         	new_world	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      	return-ty   	         @PREFIX@_new_world	      name   	         
init_world	      name   	         bool	      	return-ty   	         @PREFIX@_init_world	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      func   	           	         	         	         create_strands	      name   	         bool	      	return-ty   	         @PREFIX@_create_strands	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      func   	           	         	         	         run	      name   	         uint32_t	      	return-ty   	         @PREFIX@_run	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	         	maxNSteps	      name   	         uint32_t	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	         	          	      params   	           	      #   	      &   	         	      func   	           	      ,   	         	         shutdown	      name   	         void	      	return-ty   	         @PREFIX@_shutdown	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      func   	           	         	         	         num_strands	      name   	         uint32_t	      	return-ty   	         @PREFIX@_num_strands	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	           	         	         	         num_active_strands	      name   	         uint32_t	      	return-ty   	         @PREFIX@_num_active_strands	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	           	         	         	         num_stable_strands	      name   	         uint32_t	      	return-ty   	         @PREFIX@_num_stable_strands	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	           	         	         	         
any_errors	      name   	         bool	      	return-ty   	         @PREFIX@_any_errors	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	           	         	         	         
get_errors	      name   	         *	      kind   	         char	      arg   	           	         	         	      	return-ty   	         @PREFIX@_get_errors	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	           	      %   	         	         verbose	      name   	         bool	      	return-ty   	         @PREFIX@_get_verbose	      name   	         wrld	      name   	         *	      kind   	         @PREFIX@_world_t	      arg   	           	         	         	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	           	          	      params   	           	         	         	         	      get   	         void	      	return-ty   	         @PREFIX@_set_verbose	      name   	         wrld	      name   	         @PREFIX@_world_t*	      param-ty   	         world	           	          	      attrbs   	           	         	         	         	         mode	      name   	         bool	      param-ty   	         out	           	          	      attrbs   	           	         	         	         	           	         	          	      params   	           	         	          	         	      set   	           	      &   	      E   	         	           	      J   	      t   	      �   	      �   	      �   	        	     (   	     Y   	     }   	      inputs   params   func   runtime   outputs  �/*---------- begin lib-h-head.in ----------*/
/*! \file @H_FILE@
 *
 * C interface to library generated from @SRCFILE@.
 *
 * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@
 * Version: @DIDEROTC_VERSION@
 */

#ifndef @H_DEFINE@
#define @H_DEFINE@

#define @DIDEROT_FLOAT_PRECISION@
#define @DIDEROT_INT_PRECISION@
#define @DIDEROT_TARGET@

#include "diderot/config.h"

#if defined(HAVE_STDBOOL_H)
#  include <stdbool.h>
#elif !defined(__bool_true_false_are_defined)
#  ifndef HAVE__BOOL
#    ifdef __cplusplus
       typedef bool _Bool;
#    else
#      define _Bool signed char
#    endif
#  endif
#  define bool _Bool
#  define false 0
#  define true 1
#  define __bool_true_false_are_defined 1
#endif

#include <stdint.h>
#include <string.h>
#include "teem/nrrd.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct @PREFIX@_struct_world @PREFIX@_world_t;
/*---------- end lib-h-head.in ----------*/
  �/*---------- begin lib-h-par-extras.in ----------*/
//! get the number of hardware cores
//! \return the number of cores on the system
uint32_t @PREFIX@_set_num_cores (@PREFIX@_world_t *wrld);

//! set the number of workers.  The value should be between 0 and the number of
//! hardware cores.  Note that this function should be called after @PREFIX@_init_world
//! and before @PREFIX@_create_strands.
//! \param wrld the world-state of the Diderot program
//! \param nWorkers the requested number of workers; 0 means set the number of
//!        workers to the number of cores.
//! \return true if there are any errors
bool @PREFIX@_set_num_workers (@PREFIX@_world_t *wrld, uint32_t nWorkers);

//! get the number of workers.
//! \param wrld the world-state of the Diderot program
//! \return the number of workers
uint32_t @PREFIX@_get_num_workers (@PREFIX@_world_t *wrld);
/*---------- end lib-h-par-extras.in ----------*/
  �/*---------- begin lib-h-body.in ----------*/

/***** World query operations *****/

//! Return the total number of strands (active+stable) in the world
uint32_t @PREFIX@_num_strands (@PREFIX@_world_t *wrld);

//! Return the total number of active strands
uint32_t @PREFIX@_num_active_strands (@PREFIX@_world_t *wrld);

//! Return the total number of stable strands
uint32_t @PREFIX@_num_stable_strands (@PREFIX@_world_t *wrld);

//! Return true if there are any recorded error conditions
bool @PREFIX@_any_errors (@PREFIX@_world_t *wrld);

//! Return the pending error message (if any).  This call clears the pending error
//! state.
char *@PREFIX@_get_errors (@PREFIX@_world_t *wrld);

/***** Program running operations *****/

//! Allocate the program's world
//! \return the new world or NULL if there are any errors
@PREFIX@_world_t *@PREFIX@_new_world ();

//! Initialize the execution state for the world.  This includes allocating processor
//! and GPU resources for parallel execution.
//! \param wrld the world-state of the Diderot program
//! \return true if there are any errors
bool @PREFIX@_init_world (@PREFIX@_world_t *wrld);

//! Initiaize the globals and create the initial set of strands
//! \param wrld the world-state of the Diderot program
//! \return true if there are any errors
bool @PREFIX@_create_strands (@PREFIX@_world_t *wrld);

//! Run the Diderot program
//! \param wrld the world-state of the Diderot program
//! \param maxNSteps the limit on the number of super steps; 0 means unlimited
//! \return the number of steps taken.
uint32_t @PREFIX@_run (@PREFIX@_world_t *wrld, uint32_t maxNSteps);

//! shutdown and deallocate the world
void @PREFIX@_shutdown (@PREFIX@_world_t *wrld);

/***** Runtime options *****/

//! Set verbose mode
void @PREFIX@_set_verbose (@PREFIX@_world_t *wrld, bool mode);

//! Get verbose mode
bool @PREFIX@_get_verbose (@PREFIX@_world_t *wrld);
/*---------- end lib-h-body.in ----------*/
   �/*---------- begin lib-h-foot.in ----------*/

#ifdef __cplusplus
}
#endif

#endif /* !@H_DEFINE@ */
/*---------- end lib-h-foot.in ----------*/
	   	  �  �  �  �  �  �  �  �  �  �      	   �  �    �D$H� �D$;|$��   �t$P�\$T�E�u �  ��_�X�_�X�_�X�_�X�_�X�_�X�_�X�_ �X �_$�X$�_(�X(�_,�@,�G0�D$P�G4�\$T�_8�O<�W@�GD  �M�OH�U�WL�]�_P�G�GT��_H�U�M�D$H�t$L�   �t$���   ��X�d$H�  ��0����D$;|$w"�)�l$H�L$L�   �   �t$��  �d$H�V  ��������D$;|$w'�
�L$H�T$L�   �   �   �t$��D  �d$H�  �������D$;|$w)�+�u �S�K�[�t$H�l$L�   �D$���  �d$H��  ����|����D$;|$w"�)�l$H�L$L�   �   �t$���  �d$H�  ���D����D$;|$w'��L$H�\$L�   �   �   �t$���  �d$H�e  ;|$�Y  ���  �G   �G   �p,�G  ��O�W�W�G  �G   �G    �G$  �^�_(�o�o,�G0  �O(�O4�G8   �G<  �P$�W@�_4�_D�GH  �h �oL�O@�OP�GT  �GX	   �WL�W\�G`  �^�_d�oX�oh�Gl  �Od�Op�Gt   �Gx  �P�W|�_p���   Ǉ�     Ǉ�   	   �o|���   Ǉ�     ���   ���   �P(���   Ǉ�     Ǉ�      ���   ���   Ǉ�     �n���   ���   ���   Ǉ�     Ǉ�      Ǉ�      Ǉ�     �V���   ���   ���   Ǉ�     ���   ���   Ǉ�      Ǉ�     ���   ���   ���   ���   Ǉ�     �_���   ���   ���   Ǉ�     �H���   ���   ���   Ǉ�     �X��   ���   ��  Ǉ    �H��  ��   ��  Ǉ    �X��  ��  ��  Ǉ     �H��$  ��  ��(  Ǉ,    �X��0  ��$  ��4  Ǉ8    ���<  ��0  ��@  ǇD    ǇH  	   ��<  ��L  ǇP  �  �n ��T  �N��X  �V��\  �^��`  ��H  ��d  Ǉh  �   ��T  ��l  �p0��l  �P<�H8�X4��p  ��� �D$H   �D$L   �T$ ���T$ �d$H�codegen/fragments.sml  1p�Fragments"5DCA nff5Cpa"libHFoot"4��nC"string"0 pa"libHBody"4�%pa"libHParExtras"4�%pa"libHHead"4�%pa"jsonBody"4aAnBp"�:�C2`3��/BLN'�"0N00sABi1�A 5C�B��������N