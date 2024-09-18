mkdir mahmoud
mkdir biocomputing && cd biocomputing
wget https://raw.githubusercontent.com/josoga2/dataset-repos/main/wildtype.fna
wget https://raw.githubusercontent.com/josoga2/dataset-repos/main/wildtype.gbk
wget https://raw.githubusercontent.com/josoga2/dataset-repos/main/wildtype.gbk
mv wildtype.fna ../mahmoudrm
rm wildtype.gbk.1
grep -i -o 'tatatata' wildtype.fna && echo 'mutant' || grep -i -o 'tata' wildtype.fna && echo 'wildtype'
(grep -i -o 'tatatata' wildtype.fna && echo 'mutant' || (grep -i -o 'tata' wildtype.fna && echo 'wildtype')) > output.txt
wget -q -O - "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=NG_007491.3&rettype=fasta" > Foxp2_gene.fasta
grep -v "^>" Foxp2_gene.fasta | wc -l 
grep -o "A" Foxp2_gene.fasta | wc -l
grep -o "G" Foxp2_gene.fasta | wc -l
grep -o "C" Foxp2_gene.fasta | wc -l
grep -o "T" Foxp2_gene.fasta | wc -l
grep -v "^>" gene.fasta | tr -d '\n' | grep -o "[GCgc]" | wc -l | awk -v total=$(grep -v "^>" gene.fasta | tr -d '\n' | wc -c) '{printf "%.2f\n", ($1 / total) * 100}'
touch mahmoud.fastaecho
echo "A:191129 ,G:109310,C:105476,T:208550" >> mahmoud.fasta

