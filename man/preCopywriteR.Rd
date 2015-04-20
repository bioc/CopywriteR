\name{preCopywriteR}
\alias{preCopywriteR}
\title{
CopywriteR: DNA copy number detection from off-target sequence data
}
\description{
<<<<<<< HEAD
preCopywriteR is used to generate helper files (blacklist, bin region, GC-content,
and mappability .bed files) for the desired bin size from pre-assembled 1 kb 
helper files. These binary 1 kb helper files are available at
https://github.com/PeeperLab/CopywriteR for hg19, mm9 and mm10 reference genomes.
}
\usage{
preCopywriteR(black.GC.mapa.folder, output.folder, bin.size, reference)
}
\arguments{
\item{black.GC.mapa.folder}{
the path to the 1kb helper files.
}
=======
preCopywriteR is used to generate helper files (blacklist, bin region,
GC-content, and mappability .bed files) for the desired bin size from
pre-assembled 1 kb helper files. These binary 1 kb helper files are available at
\url{https://github.com/PeeperLab/CopywriteR} for hg18, hg19, hg38, mm9 and mm10
reference genomes.
}
\usage{
preCopywriteR(output.folder, bin.size, ref.genome, prefix = "")
}
\arguments{
>>>>>>> master
\item{output.folder}{
the path to the output folder.
}
\item{bin.size}{
desired bin length (in bp) for which helper files should be generated.
}
<<<<<<< HEAD
\item{reference}{
the reference genome for which helper files should be generated, e.g. hg19, mm9
or mm10.
}
}
\details{
Currently helper files can only be generated for  human (hg19). We are
currently implementing mm10 and will be available in the next release.

Helper files can only be generated for a bin.size that is a multiple of 1000bp. 
}
\value{
\item{blacklist.bed}{
a .bed file containing blacklisted regions of known CNVs.
}
\item{bins.bed}{
a .bed file containing the bins for the specified bin size
}
\item{GC_content.bed}{
a .bed file containing the GC-content for the specified bin size.
}
\item{mappability.bed}{
a .bed file containing the mappability for the specified bin size.
=======
\item{ref.genome}{
the reference genome for which helper files should be generated, e.g. hg18,
hg19, hg38, mm9 or mm10.
}
\item{prefix}{
the prefix that is used for chromosome notation. Standard notation is "1", "2",
..., "X", "Y". For "chr1", ... notation, use prefix = "chr".
}
}
\details{
Currently helper files can be generated for human (hg18, hg19 and hg38) and
mouse (mm9 and mm10) reference genomes.

Helper files can only be generated for a bin.size that is a multiple of 1000 bp. 
}
\value{
\item{blacklist.rda}{
an R data file containing a GRanges object with blacklisted regions of known
CNVs.
}
\item{GC_mappability.rda}{
an R data file containing a GRanges object with the mappability and GC-content
for bins of the specified size.
>>>>>>> master
}
}
\references{
CopywriteR: DNA copy number detection from off-target sequence data. Thomas
Kuilman, Arno Velds, Kristel Kemper, Marco Ranzani, Lorenzo Bombardelli,
Marlous Hoogstraat, Ekaterina Nevedomskaya, Guotai Xu, Julian de Ruiter,
Martijn P. Lolkema, Bauke Ylstra, Jos Jonkers, Sven Rottenberg, Lodewyk F.
Wessels, David J. Adams, Daniel S. Peeper, Oscar Krijgsman. Submitted for
publication. 
}
\author{
Oscar krijgsman (o.krijgsman@nki.nl)\cr
Thomas Kuilman (t.kuilman@nki.nl)
}
\examples{
\dontrun{
preCopywriteR("/PATH/TO/HG19_1KB_FOLDER/", "./", 20000, "hg19")
}
}