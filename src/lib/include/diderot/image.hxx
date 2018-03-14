/*! \file image.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_IMAGE_HXX_
#define _DIDEROT_IMAGE_HXX_

#include <string>
#include <teem/nrrd.h>

#ifndef _DIDEROT_BASE_HXX_
#include "base.hxx"
#endif
#ifndef _DIDEROT_UTIL_HXX_
#include "util.hxx"
#endif
#ifndef _DIDEROT_WORLD_HXX_
#include "world.hxx"
#endif

namespace diderot {

    namespace __details {

      //! clamp an index between 0 and size-1
        inline int index_clamp (int size, int idx)
        {
//            return (idx < 0) ? 0 : (idx < size) ? idx : (size-1);
            idx = (idx < 0) ? 0 : (idx < size) ? idx : (size-1);
            assert ((0 <= idx) && (idx < size));
            return idx;
        }

      // mirror an index if it is out of bounds
        inline int index_mirror (int size, int idx)
        {
            if (idx < 0) {
                idx = -1 - idx;
            }
            if (idx < size) {
                assert ((0 <= idx) && (idx < size));
                return idx;
            }
          // here size <= idx
            int div = idx / size;
            int rem = idx - size * div;
            idx = (div & 1) ? (size - 1) - rem : rem;
            assert ((0 <= idx) && (idx < size));
            return idx;
        }

      // wrap an index if it is out of bounds
        inline int index_wrap (int size, int idx)
        {
            if (idx < 0) {
                return (size - 1) + (idx + 1) % size;
            }
            else if (idx < size) {
                return idx;
            }
            else {
                return idx % size;
            }
        }

        template <typename REAL>
        struct image1d {
            uint32_t            _dim;           //!< dimension (== 1)
            uint32_t            _size[1];
            size_t              _dataSzb;       //!< size of data in bytes
            void                *_data;
            REAL                _s;             //!< scaling from world-space to image-space
            REAL                _t;             //!< translation from world-space to image-space

            image1d (uint32_t sz1, size_t nbytes, void *data)
              : _dim(1), _size{sz1}, _dataSzb(nbytes), _data(data)
            { }
            ~image1d () { }

          //! free the underlying storage for the image and nullify the data pointer
            void free ()
            {
                if (this->_data != nullptr) {
                    std::free (this->_data);
                    this->_data = nullptr;
                }
            }

        };

        template <typename REAL>
        struct image2d {
            uint32_t            _dim;           //!< dimension (== 2)
            uint32_t            _size[2];       //!< sizes (fast to slow)
            size_t              _dataSzb;       //!< size of data in bytes
            void                *_data;
            mat2x2<REAL>        _w2i;           //!< affine tranform from world space to index space.
                                                //!  This is the inverse of the index to world-space
                                                //!  transform that is loaded from the Nrrd file.
            real2<REAL>          _tVec;         //!< translation part of world to index transform
            mat2x2<REAL>        _w2iT;          //!< transpose w2i

            image2d (uint32_t sz1, uint32_t sz2, size_t nbytes, void *data)
              : _dim(2), _size{sz1, sz2}, _dataSzb(nbytes), _data(data)
            { }
            ~image2d () { }

          //! free the underlying storage for the image and nullify the data pointer
            void free ()
            {
                if (this->_data != nullptr) {
                    std::free (this->_data);
                    this->_data = nullptr;
                }
            }

        };

        template <typename REAL>
        struct image3d {
            uint32_t            _dim;           //!< dimension (== 3)
            uint32_t            _size[3];       //!< sizes (fast to slow)
            size_t              _dataSzb;       //!< size of data in bytes
            void                *_data;
            mat3x3<REAL>        _w2i;           //!< affine tranform from world space to index space.
                                                //!  This is the inverse of the index to world-space
                                                //!  transform that is loaded from the Nrrd file.
            real3<REAL>          _tVec;         //!< translation part of world to index transform
            mat3x3<REAL>        _w2iT;          //!< transpose w2i

            image3d (uint32_t sz1, uint32_t sz2, uint32_t sz3, size_t nbytes, void *data)
              : _dim(3), _size{sz1, sz2, sz3}, _dataSzb(nbytes), _data(data)
            { }
            ~image3d () { }

          //! free the underlying storage for the image and nullify the data pointer
            void free ()
            {
                if (this->_data != nullptr) {
                    std::free (this->_data);
                    this->_data = nullptr;
                }
            }

        };

    } // namespace __details

    template <typename TY> struct image_traits;
    // using value_type = TY;
    // static const int type = ...;

  // 1D images with sample type TY and Diderot real type REAL
    template <typename REAL, typename TY>
    class image1d {
      public:

        using traits = image_traits<TY>;

        image1d () : _img(nullptr) { }
        image1d (const image1d &img) : _img(img._img) { }
        ~image1d () { }

        bool load (struct world_base *wrld, std::string const &name);
        bool load (struct world_base *wrld, const Nrrd *nin);

        bool inside (int idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        REAL world2image () const;
        REAL translate () const;

        void free_data () { this->_img->free(); }

      private:
        __details::image1d<REAL> *_img;
    };

  // 2D images with sample type TY and Diderot real type REAL
    template <typename REAL, typename TY>
    class image2d {
      public:

        using traits = image_traits<TY>;

        image2d () : _img(nullptr) { }
        image2d (const image2d &img) : _img(img._img) { }
        ~image2d () { }

        bool load (struct world_base *wrld, std::string const &name);
        bool load (struct world_base *wrld, const Nrrd *nin);

        bool inside (array<int,2> const & idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        mat2x2<REAL> const &world2image () const;
        real2<REAL> const &translate () const;

        void free_data () { this->_img->free(); }

      private:
        __details::image2d<REAL> *_img;
    };

  // 3D images with sample type TY and Diderot real type REAL
    template <typename REAL, typename TY>
    class image3d {
      public:

        using traits = image_traits<TY>;

        image3d () : _img(nullptr) { }
        image3d (const image3d &img) : _img(img._img) { }
        ~image3d () { }

        bool load (struct world_base *wrld, std::string const &name);
        bool load (struct world_base *wrld, const Nrrd *nin);

        bool inside (array<int,3> const &idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        mat3x3<REAL> const &world2image () const;
        real3<REAL> const &translate () const;

        void free_data () { this->_img->free(); }

      private:
        __details::image3d<REAL> *_img;
    };

  /***** image1d inline functions *****/
    template <typename REAL, typename TY>
    inline bool image1d<REAL,TY>::inside (int idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx) && (idx < static_cast<int>(this->_img->_size[0] - s)));
    }

    template <typename REAL, typename TY>
    inline int image1d<REAL,TY>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image1d<REAL,TY>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image1d<REAL,TY>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline uint32_t image1d<REAL,TY>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY>
    inline const TY *image1d<REAL,TY>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY>
    inline REAL image1d<REAL,TY>::operator[] (uint32_t idx) const
    {
        assert ((0 <= idx) && (idx < this->_img->_dataSzb/sizeof(REAL)));
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY>
    inline REAL image1d<REAL,TY>::world2image () const
    {
        return this->_img->_s;
    }

    template <typename REAL, typename TY>
    inline REAL image1d<REAL,TY>::translate () const
    {
        return this->_img->_t;
    }


  /***** image2d inline functions *****/
    template <typename REAL, typename TY>
    inline bool image2d<REAL,TY>::inside (array<int,2> const &idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx[0]) && (idx[0] < static_cast<int>(this->_img->_size[0] - s))
            &&  (s-1 <= idx[1]) && (idx[1] < static_cast<int>(this->_img->_size[1] - s)));
    }

    template <typename REAL, typename TY>
    inline int image2d<REAL,TY>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image2d<REAL,TY>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image2d<REAL,TY>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline uint32_t image2d<REAL,TY>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY>
    inline const TY *image2d<REAL,TY>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY>
    inline REAL image2d<REAL,TY>::operator[] (uint32_t idx) const
    {
        assert ((0 <= idx) && (idx < this->_img->_dataSzb/sizeof(REAL)));
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY>
    inline mat2x2<REAL> const &image2d<REAL,TY>::world2image () const
    {
        return this->_img->_w2i;
    }

    template <typename REAL, typename TY>
    inline real2<REAL> const &image2d<REAL,TY>::translate () const
    {
        return this->_img->_tVec;
    }


  /***** image2d inline functions *****/
    template <typename REAL, typename TY>
    inline bool image3d<REAL,TY>::inside (array<int,3> const &idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx[0]) && (idx[0] < static_cast<int>(this->_img->_size[0] - s))
            &&  (s-1 <= idx[1]) && (idx[1] < static_cast<int>(this->_img->_size[1] - s))
            &&  (s-1 <= idx[2]) && (idx[2] < static_cast<int>(this->_img->_size[2] - s)));
    }

    template <typename REAL, typename TY>
    inline int image3d<REAL,TY>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image3d<REAL,TY>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline int image3d<REAL,TY>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY>
    inline uint32_t image3d<REAL,TY>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY>
    inline const TY *image3d<REAL,TY>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY>
    inline REAL image3d<REAL,TY>::operator[] (uint32_t idx) const
    {
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY>
    inline mat3x3<REAL> const &image3d<REAL,TY>::world2image () const
    {
        return this->_img->_w2i;
    }

    template <typename REAL, typename TY>
    inline real3<REAL> const &image3d<REAL,TY>::translate () const
    {
        return this->_img->_tVec;
    }

} // namespace diderot

#include "image-inst.hxx"

#endif //! _DIDEROT_IMAGE_HXX_
