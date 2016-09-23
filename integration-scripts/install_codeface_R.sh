#!/bin/sh
# Copyright Roger Meier <roger@bufferoverflow.ch>
# SPDX-License-Identifier:	Apache-2.0 BSD-2-Clause GPL-2.0+ MIT WTFPL

echo "Providing R libraries"

sudo DEBIAN_FRONTEND=noninteractive apt-get --force-yes -qqy install r-base r-base-dev \
	r-cran-zoo r-cran-xts \
	r-cran-xtable r-cran-reshape r-cran-stringr r-cran-scales \
	r-cran-scales r-cran-rmysql r-cran-rcurl r-cran-mgcv \
	r-cran-rjson r-cran-testthat libx11-dev libssl-dev libssh2-1-dev

sudo Rscript packages.r
