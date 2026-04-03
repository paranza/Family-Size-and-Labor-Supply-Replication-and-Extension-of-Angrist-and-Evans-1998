📊 **Data Source**

The dataset used in this project comes from the Angrist Data Archive:

🔗 https://economics.mit.edu/people/faculty/josh-angrist/angrist-data-archive

Navigate to:

Angrist and Evans (1998)

Download the corresponding ZIP file containing the datasets.

📥 **Importing Data into R**

Create a folder called 'dati' and move the 2 .sas files inside of it

The final structure of the folder should be:
📂Econometrics-Assingment/
 ├── Econometrics-Assingment-R.Rproj
 ├── README.md
 ├── file_da_usare.R
 ├── etc...
 └── 📂 dati/                
      ├── dataset_1.sas       
      └── dataset_2.sas  
      
(
il motivo per cui dovere fare questo è che sennò tutti quanti teniamo i file in un percorso diverso,
quindi quando qualcuno fa runnare il codice di un altro non va 

basterà fare: 
library(haven)    #serve sta libreria
dati = read_sas('dati/m_d_806.sas7bdat') #il file è molto grande quindi ci mette un po, ma poi appare sulla destra
)
