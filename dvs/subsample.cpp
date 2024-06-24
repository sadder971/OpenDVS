#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

void double2float(const arma::mat& a, float* b) {
    int N = a.n_elem;
    for (int i = 0; i < N; i++) {
        b[i] = static_cast<float>(a[i]);
    }
}

void float2double(float* a, arma::mat& b) {
    int N = b.n_elem;
    for (int i = 0; i < N; i++) {
        b[i] = static_cast<double>(a[i]);
    }
}

// ######################################################################
// kernel: 1 5 10 10 5 1
void lowPass6yDecY(float* sptr, float* rptr, int w, int hs)
{
  int x, y;
  int hr = hs / 2;
  if (hr == 0) hr = 1;

  /* if (hs <= 1)
     result = src;
     else 
  */ 
  if (hs == 2)
    for (x = 0; x < w; ++x)
      {
        // use kernel [1 1]^T / 2
        *rptr++ = (sptr[0] + sptr[1]) / 2.0;
        sptr += 2;
      }
  else if (hs == 3)
    for (x = 0; x < w; ++x)
      {
        // use kernel [1 2 1]^T / 4
        *rptr++ = (sptr[0] + sptr[1] * 2.0 + sptr[2]) / 4.0;
        sptr += 3;
      }
  else // general case with hs >= 4
    for (x = 0; x < w; ++x)
      {
        // top most point - use kernel [10 10 5 1]^T / 26
        *rptr++ = ((sptr[0] + sptr[1]) * 10.0 + 
		   sptr[2] * 5.0 + sptr[3]) / 26.0;
        //++sptr;
        
        // general case
        for (y = 0; y < (hs - 5); y += 2)
          {
            // use kernel [1 5 10 10 5 1]^T / 32
            *rptr++ = ((sptr[1] + sptr[4])  *  5.0 +
                       (sptr[2] + sptr[3])  * 10.0 +
                       (sptr[0] + sptr[5])) / 32.0;
            sptr += 2;
          }
        
        // find out how to treat the bottom most point
        if (y == (hs - 5))
          {
            // use kernel [1 5 10 10 5]^T / 31
	    *rptr++ = ((sptr[1] + sptr[4])  *  5.0 +
		       (sptr[2] + sptr[3])  * 10.0 +
		       sptr[0])            / 31.0;
            sptr += 5;
          }
        else
          {
            // use kernel [1 5 10 10]^T / 26
            *rptr++ = ( sptr[0] + sptr[1]  *  5.0 +
			(sptr[2] + sptr[3]) * 10.0) / 26.0;
            sptr += 4;
          }
      }
}

// ######################################################################
// kernel: 1 5 10 10 5 1
void lowPass6xDecX(float* sptr, float* rptr, int ws, int h)
{
  int x,y;
  const int h2 = h * 2, h3 = h * 3, h4 = h * 4, h5 = h * 5;
  int wr = ws / 2;
  if (wr == 0) wr = 1;

  /* if (ws <= 1)
     result = src;
     else */
  if (ws == 2)
    for (y = 0; y < h; ++y)
      {
        // use kernel [1 1] / 2
        *rptr++ = (sptr[0] + sptr[h]) / 2.0;
        ++sptr;
      }
  else if (ws == 3)
    for (y = 0; y < h; ++y)
      {
        // use kernel [1 2 1] / 4
        *rptr++ = (sptr[0] + sptr[h] * 2.0 + sptr[h2]) / 4.0;
        ++sptr;
      }
  else // general case for ws >= 4
    {
      // left most point - use kernel [10 10 5 1] / 26
      for (y = 0; y < h; ++y)
        {
          *rptr++ = ((sptr[0] + sptr[h]) * 10.0 + 
		     sptr[h2] * 5.0 + sptr[h3]) / 26.0;
          ++sptr;
        }
      sptr -= h;
      
      // general case
      for (x = 0; x < (ws - 5); x += 2)
        {
          for (y = 0; y < h; ++y)
            {
              // use kernel [1 5 10 10 5 1] / 32
              *rptr++ = ((sptr[h]  + sptr[h4])  *  5.0 +
                         (sptr[h2] + sptr[h3])  * 10.0 +
                         (sptr[0]  + sptr[h5])) / 32.0;
              ++sptr;
            }
          sptr += h;
        }
        
      // find out how to treat the right most point
      if (x == (ws - 5))
        for (y = 0; y < h; ++y)
          {
            // use kernel [1 5 10 10 5] / 31
            *rptr++ = ((sptr[h]  + sptr[h4])  *  5.0 +
                       (sptr[h2] + sptr[h3])  * 10.0 +
		       sptr[0]) / 31.0;
            ++sptr;
          }
      else
        for (y = 0; y < h; ++y)
          {
            // use kernel [1 5 10 10] / 26
            *rptr++ = ( sptr[0]  + sptr[h]   * 5.0 + 
			(sptr[h2] + sptr[h3]) * 10.0) / 26.0;
            ++sptr;
          }
    }
}

// [[Rcpp::export]]
arma::mat Subsample(const arma::mat& img) {
    int w = img.n_cols;
    int h = img.n_rows;
    int wr = w / 2; if (wr == 0) wr = 1;
    int hr = h / 2; if (hr == 0) hr = 1;

    arma::mat out(hr, wr, arma::fill::zeros);

    if ((w > 10) && (h > 10)) {  
        float* imgF = new float[h * w];
        float* decx = new float[h * wr];
        float* decxy = new float[hr * wr];

        double2float(img, imgF);
        lowPass6xDecX(imgF, decx, w, h);
        lowPass6yDecY(decx, decxy, wr, h);

        float2double(decxy, out);

        delete[] imgF;
        delete[] decx;
        delete[] decxy;
    } else {
        out = img;
    }

    return out;
}

