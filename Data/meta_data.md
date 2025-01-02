

* En 2016, la variable sur la déqualification n'avait pas été posée

* Je vous joins également la liste des variables (d'une année à l'autre les noms des variables restent les mêmes si on excepte les 3premiers caractères faisant référence aux années d'enquêtes

* Enfin, ci dessous le code qui vous permet de voir comment les variables ont été regroupées

proc format; 
      VALUE EDUC 1,2 = "Secondaire I" 3-6 ="Secondaire II" 7-9 = "Tertiaire";
      value origine 
      8207 = "Allemagne"
      8212 = "France"
      8218 = "Italie"
      8229 = "Autriche"
      8231 = "Portugal"
      8236 = "Espagne"
      8204, 8205, 8206, 8242,8250, 8260, 8211, 8214,8216, 8240, 8261,8262, 8223,8224, 8227,8230, 8232,8244,8243, 8251,
            8215, 8234, 8217,8228, 8222 ="Autre UE/AELE"
            8601,8407,8423,8539,8439,8514,8515,8427,8607,8239 ="Autre OCDE"
      8300-8399 = "Afrique"
      8400-8406, 8408-8422, 8424-8426, 8428-8438,8440-8499 ="Amerique latine"
      8500-8513, 8516-8538, 8540-8600, 8602-8606, 8608-8700= "Asie"
      other = "Autre Europe";
      value skills 1,2 ="Oui" 3,4 = "Non";
      value seul 1 ="Oui" 2-99 ="Non";

A noter que les tableaux sont pondérés par une variable de pondération (Weight_20xx). Je suppose que si vous faites une représentation reposant sur les individus, vous n'avez pas besoin de cette variable, mais je l'ai insérée.
