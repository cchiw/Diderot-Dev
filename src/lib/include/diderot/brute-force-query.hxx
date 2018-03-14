/*! \file brute-force-query.hxx
 *
 * \author John Reppy
 *
 * This is a brute-force implementation (i.e., O(n^2) of spatial queries for testing
 * purposes.  It is designed to match the kdtree API (see kdtree.hxx), but is implementend
 * using strand-to-strand comparisons.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_BRUTE_FORCE_QUERY_HXX_
#define _DIDEROT_BRUTE_FORCE_QUERY_HXX_

#ifndef _DIDEROT_BASE_HXX_
#include "diderot/base.hxx"
#endif

#include "query-util.hxx"

namespace diderot {

  //! kdtrees are parameterized over the spatial dimension D, the real type, and the
  //! strand_array type
    template <const uint32_t D, typename REAL, typename SA>
    struct kdtree {
	using strand_t = typename SA::strand_t;
	using index_t = typename SA::index_t;

	const SA	*_strands;

	explicit kdtree (const SA *strands) : _strands(strands) { }
	~kdtree () { }

	void rebuild () { }

	dynseq<index_t> sphere_query (const strand_t * self, const REAL pos[D], REAL radius);
    };

  // sphere_query
  //
    template <const uint32_t D, typename REAL, typename SA>
    dynseq<typename kdtree<D,REAL,SA>::index_t> kdtree<D,REAL,SA>::sphere_query (
	const kdtree<D,REAL,SA>::strand_t *self, const REAL pos[D], REAL radius)
    {
	dynseq<index_t> result;

      // return empty sequence on empty sphere
	if (radius < 0.0) {
	    return result;
	}

	const SA *strands = this->_strands;
	REAL radius2 = radius * radius;
	uint32_t inIdx = strands->in_state_index();

	for (index_t ix = strands->begin_alive();
	    ix != strands->end_alive();
	    ix = strands->next_alive(ix))
	{
	    const strand_t *strand = strands->strand(ix);
	    if ((self != strand)
	    && __details::within_sphere<D,REAL>(strand->pos(inIdx), pos, radius2)) {
	      // add the strand to the result list
		result.append (strands->id(ix));
	    }
	}
	
	return result;
    }

} // namespace diderot

#endif // !_DIDEROT_BRUTE_FORCE_QUERY_HXX_
