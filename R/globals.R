globalVariables(unique(c(
  # col_ipr_classe: 
  "classe_ipr", "cli_altitude_min", "cli_borne_inf", "cli_borne_sup", "cli_classe", "cli_libelle", 
  # get_liste_ope_station: 
  "dept", "ope_commentaire", "ope_date", "ope_eta_id", "ope_id", "operateur_peche", 
  # get_liste_stations_dpt: 
  "code_sta_pp", "dept", "sta_libelle_sandre", 
  # mef_ajouter_codes_sandre_pp: 
  "code_sandre_pp", "point_prelevement", "pop_code_sandre", "pop_id", "sta_code_sandre", "sta_coordonnees_x", "sta_coordonnees_y", "sta_id", "sta_libelle_sandre", "station", 
  # mef_ajouter_facies: 
  "fac_gra_id_accessoire", "fac_gra_id_dominante", "fac_ode_ope_id", "fac_tyc_id", "fac_tyf_id", "fac_ved_id", "facies", "ref_granulometrie", "ref_type_colmatage", "ref_type_facies", "ref_vegetation_dominante", 
  # mef_ajouter_habitat: 
  "hab_omr_id", "hab_sin_id", "hab_tya_id_abri_vegetal_aquatique", "hab_tya_id_abris_rocheux", "hab_tya_id_embacles_souches", "hab_tya_id_sous_berges", "hab_tya_id_trous_fosses", "hab_tya_id_vegetation_bordure", "hab_tys_id_vegetation_aquatique", "habitat", "ode_hab_id", "ode_observation_vegetation", "ode_ope_id", "ode_profondeur_moyenne_station", "operation_donnees_environnementales", "ref_ombrage_riviere", "ref_sinuosite", "ref_type_abondance", "ref_type_abondance_saumon", 
  # mef_ajouter_infos_context: 
  "ope_amplitude_thermique_air_station_cerema", "ope_cap_id", "ope_commentaire", "ope_durete_totale", "ope_espece_ciblee", "ope_eta_id", "ope_id", "ope_niq_id", "ope_niveau_typologique_theorique", "ope_pente_ligne_eau", "ope_precipitation_bassin_versant_cerema", "ope_section_mouillee", "ope_surface_calculee", "ope_temp_air_bassin_versant_cerema", "ope_temp_max_moyenne_eau", "ope_temperature_air_station_cerema", "operation", "ref_categorie_piscicole", "ref_etat_avancement", "ref_niveau_qualification", 
  # mef_creer_table_base_fiches: 
  "commanditaire_ofb", "obj_libelle", "ope_date", "operateur_ofb", "validation_ofb", 
  # mef_creer_table_fiches: 
  "commanditaire_ofb", "esp_code_alternatif", "esp_id", "esp_nom_commun", "lop_effectif", "lop_esp_id", "lop_id", "lot_poissons", "mop_id", "mop_libelle", "obj_libelle", "odp_coh_id", "odp_mop_id", "odp_ope_id", "odp_ted_id", "odp_tur_id", "ope_commentaire", "ope_eta_id", "ope_id", "ope_niq_id", "ope_surface_calculee", "operateur_ofb", "operation", "operation_description_peche", "ref_condition_hydrologique", "ref_espece", "ref_etat_avancement", "ref_moyen_prospection", "ref_niveau_qualification", "ref_tendance_debit", "ref_turbidite", "validation_ofb", 
  # mef_creer_table_ope_env: 
  "annee", "code_sandre_pp", "code_sta_pp", "mop_id", "mop_libelle", "odp_coh_id", "odp_mop_id", "odp_ope_id", "odp_ted_id", "odp_tur_id", "ope_commentaire", "ope_date", "ope_eta_id", "ope_id", "ope_niq_id", "ope_surface_calculee", "operation", "operation_description_peche", "passage", "pop_id", "pre_id", "pro_libelle", "ref_condition_hydrologique", "ref_etat_avancement", "ref_moyen_prospection", "ref_niveau_qualification", "ref_tendance_debit", "ref_turbidite", "sta_id", "sta_libelle_sandre", 
  # mef_creer_table_peuplement: 
  "annee", "code_sta_pp", "esp_code_alternatif", "esp_id", "esp_nom_commun", "grp_tgp_id", "lop_effectif", "lop_esp_id", "lop_id", "lot_poissons", "ope_date", "ope_id", "passage", "pre_id", "ref_espece", "ref_type_groupe_points", "sta_libelle_sandre", "tpe_libelle", 
  # mef_tab_proba_esp_ipr: 
  "annee", "code_sta_pp", "esp_code_alternatif", "esp_id", "esp_nom_commun", "ope_date", "ope_id", "ppi_param_effectif", "probabilite_presence_ipr", "ref_espece", "sta_libelle_sandre", 
  # mef_tab_protocole_pts: 
  "annee", "code_sta_pp", "grp_nombre", "grp_tgp_id", "mop_libelle", "ope_id", "pas_numero", "passage", "pro_libelle", "ref_type_groupe_points", "sta_libelle_sandre", "tpe_libelle", 
  # plot_completude_valeur: 
  ".", "annee", "code_sta_pp", "name", "ope_date", "ope_id", "pre_id", "sta_libelle_sandre", "value", 
  # plot_descript_peche_env: 
  ".", "annee", "code_sta_pp", "name", "odp_coh_id", "odp_intensite", "odp_largeur_lame_eau", "odp_longueur", "odp_maille_epuisette", "odp_nombre_anodes", "odp_nombre_epuisettes", "odp_puissance", "odp_ted_id", "odp_tension", "odp_tur_id", "ope_date", "ope_id", "ope_surface_calculee", "pre_id", "profondeur", "sta_libelle_sandre", "value", 
  # plot_facies_data: 
  ".", "annee", "code_sta_pp", "fac_gra_id_accessoire", "fac_gra_id_dominante", "fac_importance_relative", "fac_profondeur_moyenne", "fac_recouvrement_vegetation", "fac_tyc_id", "fac_tyf_id", "fac_ved_id", "name", "ope_date", "ope_id", "pch_tyf", "sta_libelle_sandre", "value", 
  # plot_habitat_data: 
  ".", "annee", "code_sta_pp", "hab_tys_id_vegetation_aquatique", "name", "ode_profondeur_moyenne_station", "ope_date", "ope_id", "sta_libelle_sandre", "value", 
  # plot_infos_context: 
  ".", "annee", "code_sta_pp", "name", "ope_date", "ope_id", "sta_libelle_sandre", "value", 
  # plot_ipr_metrique: 
  ".", "annee", "code_sta_pp", "metrique", "ope_date", "ope_id", "sta_libelle_sandre", "type", "valeur", 
  # plot_ipr_param_station: 
  ".", "annee", "code_sta_pp", "distance_mer", "name", "ope_date", "ope_id", "pop_id", "sta_libelle_sandre", "temp_janvier", "value", 
  # plot_ipr_proba_esp_station: 
  "esp_code_alternatif", "ope_date", "ope_id", "pch_size", "ppi_esp_id", "ppi_param_effectif", "ppi_valeur_probabilite", "presence", "text", "text_interactive", 
  # plot_ipr_station: 
  ".", "annee", "cli_borne_inf", "cli_borne_sup", "cli_libelle", "code_sta_pp", "ipr", "name", "ope_date", "ope_id", "sta_libelle_sandre", "value", 
  # plot_jour_peche: 
  "code_sta_pp", "ope_date", "ope_date2", "ope_id", "xmax", "xmin", 
  # plot_objectif_reseau: 
  ".", "annee", "code_sta_pp", "obj_libelle", "ope_date", "ope_id", "sta_libelle_sandre", "value", 
  # plot_peuplement_data: 
  ".", "annee", "code_sta_pp", "esp_code_alternatif", "esp_nom_commun", "lop_effectif", "ope_date", "ope_id", "pre_id", "sta_libelle_sandre", "tpe_libelle", 
  # plot_proto_prospec_passage: 
  ".", "annee", "code_sta_pp", "grp_nombre", "grp_tgp_id", "mop_libelle", "name", "ope_date", "ope_id", "pas_numero", "pro_libelle", "ref_type_groupe_points", "sta_libelle_sandre", "tpe_libelle", "value", 
  # select_infos_context: 
  "annee", "code_sta_pp", "ope_amplitude_thermique_air_station_cerema", "ope_cap_id", "ope_date", "ope_durete_totale", "ope_espece_ciblee", "ope_id", "ope_niveau_typologique_theorique", "ope_pente_ligne_eau", "ope_precipitation_bassin_versant_cerema", "ope_section_mouillee", "ope_temp_air_bassin_versant_cerema", "ope_temp_max_moyenne_eau", "ope_temperature_air_station_cerema", "sta_libelle_sandre", 
  # tab_col_class_ipr: 
  "classe_ipr", "cli_altitude_min", "cli_borne_inf", "cli_borne_sup", "cli_classe", "cli_libelle",
  # mef_creer_ttes_tables_fichesCQ:
  "cli_id"
)))