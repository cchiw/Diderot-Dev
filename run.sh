make local-install
make clean
svn up -r 5426
#svn up
export PATH=/usr/local/smlnj/bin:/usr/local:/usr/bin:$PATH
autoheader -Iconfig
 autoconf -Iconfig
./configure --with-teem=/usr/local
make local-install
