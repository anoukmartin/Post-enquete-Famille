## sexe
freq(BIEF$sexe)
BIEF <- BIEF %>% mutate(sexe = recode(as.character(sexe), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$sexe)

## iln
freq(BIEF$iln)
BIEF <- BIEF %>% mutate(iln = recode(as.character(iln), "1" = "En France (y compris outre-mer)", "2" = "À l'étranger"))
freq(BIEF$iln)

## inat1
freq(BIEF$inat1)
BIEF <- BIEF %>% mutate(inat1 = recode(as.character(inat1), "1" = "Française ? Oui", "2" = "Française ? Non"))
freq(BIEF$inat1)

## inat2
freq(BIEF$inat2)
BIEF <- BIEF %>% mutate(inat2 = recode(as.character(inat2), "1" = "Étrangère ? Oui", "2" = "Étrangère ? Non"))
freq(BIEF$inat2)

## iran
freq(BIEF$iran)
BIEF <- BIEF %>% mutate(iran = recode(as.character(iran), "1" = "Dans le même logement que maintenant", "2" = "Dans un autre logement du même arrondissement/de la même commune (COM_LOC)", "3" = "En France, dans une autre commune (y compris outre-mer) ou un autre arrondissement", "4" = "À l'étranger"))
freq(BIEF$iran)

## situat
freq(BIEF$situat)
BIEF <- BIEF %>% mutate(situat = recode(as.character(situat), "1" = "Emploi (salarié(e), à votre compte, vous travaillez sans être rémunéré(e) avec un membre de votre famille)", "NA" = "Y compris en congé maladie ou maternité, en congé parental de moins de 3 mois, en congé parental de plus de 3 moins avec un revenu compensatoire (Ex: Prepare) ou au chômage technique.", "2" = "Alternance en apprentissage", "3" = "Alternance en contrat de professionnalisation", "4" = "Études (élève, étudiant(e)) ou stage", "5" = "Chômage (inscrit(e) ou non à France Travail / Pôle emploi)", "6" = "Retraite ou préretraite", "7" = "Femme au foyer/Homme au foyer", "8" = "Autre situation"))
freq(BIEF$situat)

## sitrav
freq(BIEF$sitrav)
BIEF <- BIEF %>% mutate(sitrav = recode(as.character(sitrav), "1" = "Oui", "2" = "Non"))
freq(BIEF$sitrav)

## dejat
freq(BIEF$dejat)
BIEF <- BIEF %>% mutate(dejat = recode(as.character(dejat), "1" = "Oui", "2" = "Non"))
freq(BIEF$dejat)

## rech
freq(BIEF$rech)
BIEF <- BIEF %>% mutate(rech = recode(as.character(rech), "1" = "Oui, depuis moins d'un an", "2" = "Oui, depuis un an ou plus", "3" = "Non"))
freq(BIEF$rech)

## stat
freq(BIEF$stat)
BIEF <- BIEF %>% mutate(stat = recode(as.character(stat), "1" = "à votre compte (y compris gérant(e) de société ou chef(fe) d’entreprise salarié(e)", "2" = "salarié(e) de la fonction publique (d’État, territoriale, hospitalière)", "3" = "salarié(e) d’un autre employeur (entreprise, association, particulier, etc.)", "4" = "non rémunéré(e) mais vous travaillez avec un membre de votre famille"))
freq(BIEF$stat)

## interim
freq(BIEF$interim)
BIEF <- BIEF %>% mutate(interim = recode(as.character(interim), "1" = "Oui", "2" = "Non"))
freq(BIEF$interim)

## trans
freq(BIEF$trans)
BIEF <- BIEF %>% mutate(trans = recode(as.character(trans), "1" = "Pas de déplacement", "2" = "Marche à pied, rollers ou trottinette", "3" = "Vélo (y compris à assistance électrique)", "4" = "Deux-roues motorisé", "5" = "Voiture, camion ou fourgonnette", "6" = "Transports en commun"))
freq(BIEF$trans)

## teletravail
freq(BIEF$teletravail)
BIEF <- BIEF %>% mutate(teletravail = recode(as.character(teletravail), "1" = "Jamais", "2" = "Quelques jours par mois", "3" = "Un jour par semaine", "4" = "Deux jours par semaine", "5" = "Trois jours par semaine ou plus"))
freq(BIEF$teletravail)

## tp
freq(BIEF$tp)
BIEF <- BIEF %>% mutate(tp = recode(as.character(tp), "1" = "à temps complet", "2" = "à temps partiel à 80 % ou plus", "3" = "à temps partiel à moins de 80 %"))
freq(BIEF$tp)

## empl
freq(BIEF$empl)
BIEF <- BIEF %>% mutate(empl = recode(as.character(empl), "1" = "CDI (contrat à durée indéterminée) ou fonctionnaire", "2" = "Autre contrat (CDD, intérim, stage, etc.) de 3 mois ou plus", "3" = "Autre contrat (CDD, intérim, stage, etc.) de moins de 3 mois"))
freq(BIEF$empl)

## vardompart
freq(BIEF$vardompart)
BIEF <- BIEF %>% mutate(vardompart = recode(as.character(vardompart), "1" = "fixe, en dehors de votre domicile (hors télétravail)", "2" = "variable", "3" = "fixe, exclusivement à votre domicile"))
freq(BIEF$vardompart)

## ilt
freq(BIEF$ilt)
BIEF <- BIEF %>% mutate(ilt = recode(as.character(ilt), "1" = "Oui", "2" = "Non"))
freq(BIEF$ilt)

## ilt2
freq(BIEF$ilt2)
BIEF <- BIEF %>% mutate(ilt2 = recode(as.character(ilt2), "1" = "En France", "2" = "À l’étranger"))
freq(BIEF$ilt2)

## statav
freq(BIEF$statav)
BIEF <- BIEF %>% mutate(statav = recode(as.character(statav), "1" = "à votre compte (y compris gérant(e) de société ou chef(fe) d’entreprise salarié(e)", "2" = "salarié(e) de la fonction publique (d’État, territoriale, hospitalière)", "3" = "salarié(e) d’un autre employeur (entreprise, association, particulier, etc.)", "4" = "non rémunéré(e) mais vous travailliez avec un membre de votre famille"))
freq(BIEF$statav)

## nbsal
freq(BIEF$nbsal)
BIEF <- BIEF %>% mutate(nbsal = recode(as.character(nbsal), "1" = "Une seule personne, vous travaillez seul(e)", "2" = "Entre 2 et 10 personnes", "3" = "Entre 11 et 49 personnes", "4" = "50 personnes ou plus"))
freq(BIEF$nbsal)

## posp
freq(BIEF$posp)
BIEF <- BIEF %>% mutate(posp = recode(as.character(posp), "1" = "manœuvre, ouvrier(ouvrière) spécialisé(e)", "2" = "ouvrier(ouvrière) qualifié(e), technicien(technicienne) d'atelier", "3" = "employé(e) de bureau, de commerce, de services", "4" = "agent(e) de maîtrise (y compris administrative ou commerciale)", "5" = "technicien(ne)", "6" = "ingénieur(e), cadre d'entreprise", "7" = "agent(e) de catégorie C de la fonction publique", "8" = "agent(e) de catégorie B de la fonction publique", "9" = "agent(e) de catégorie A de la fonction publique", "10" = "dans une autre situation"))
freq(BIEF$posp)

## gali
freq(BIEF$gali)
BIEF <- BIEF %>% mutate(gali = recode(as.character(gali), "1" = "Oui, fortement limité(e)", "2" = "Oui, limité(e) mais pas fortement", "3" = "Non, pas limité(e)", "4" = "Je ne souhaite pas répondre"))
freq(BIEF$gali)

## etud
freq(BIEF$etud)
BIEF <- BIEF %>% mutate(etud = recode(as.character(etud), "1" = "Oui", "2" = "Non"))
freq(BIEF$etud)

## iletu
freq(BIEF$iletu)
BIEF <- BIEF %>% mutate(iletu = recode(as.character(iletu), "1" = "Dans le même arrondissement/la même commune que votre logement (COM_LOC)", "2" = "En France, dans une autre commune ou un autre arrondissement", "3" = "À l'étranger"))
freq(BIEF$iletu)

## dipl
freq(BIEF$dipl)
BIEF <- BIEF %>% mutate(dipl = recode(as.character(dipl), "1" = "Aucun diplôme", "2" = "CEP (certificat d’études primaires)", "3" = "BEPC, brevet élémentaire, brevet des collèges, DNB", "4" = "CAP, BEP ou diplôme de niveau équivalent", "5" = "Baccalauréat général ou technologique, brevet supérieur", "6" = "Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent", "7" = "Capacité en droit, DAEU, ESEU", "8" = "BTS, DUT, Deug, Deust, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent", "9" = "Licence, licence pro, BUT, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4", "10" = "Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé", "11" = "Doctorat de recherche (hors santé)"))
freq(BIEF$dipl)

## niv_etud
freq(BIEF$niv_etud)
BIEF <- BIEF %>% mutate(niv_etud = recode(as.character(niv_etud), "1" = "Vous êtes en cours d’études.", "2" = "Vous n'êtes jamais allé(e) à l’école ou vous l’avez quittée avant la fin du primaire.", "3" = "Vous avez interrompu votre scolarité à la fin du primaire ou avant la fin du collège.", "4" = "Vous avez suivi votre scolarité jusqu’à la fin du collège ou au-delà."))
freq(BIEF$niv_etud)

## couple
freq(BIEF$couple)
BIEF <- BIEF %>% mutate(couple = recode(as.character(couple), "1" = "Oui", "2" = "Non"))
freq(BIEF$couple)

## matr
freq(BIEF$matr)
BIEF <- BIEF %>% mutate(matr = recode(as.character(matr), "1" = "Marié(e)", "2" = "Pacsé(e)", "3" = "En concubinage ou union libre", "4" = "Veu(veuve)", "5" = "Divorcé(e)", "6" = "Célibataire"))
freq(BIEF$matr)

## nai_p1
freq(BIEF$nai_p1)
BIEF <- BIEF %>% mutate(nai_p1 = recode(as.character(nai_p1), "1" = "En France", "2" = "À l’étranger", "3" = "Je ne sais pas", "4" = "Je ne souhaite pas répondre"))
freq(BIEF$nai_p1)

## nai_p2
freq(BIEF$nai_p2)
BIEF <- BIEF %>% mutate(nai_p2 = recode(as.character(nai_p2), "1" = "En France", "2" = "À l’étranger", "3" = "Je ne sais pas", "4" = "Je ne souhaite pas répondre"))
freq(BIEF$nai_p2)

## SEPARE
freq(BIEF$SEPARE)
BIEF <- BIEF %>% mutate(SEPARE = recode(as.character(SEPARE), "1" = "...vous vous êtes séparé(e)s", "2" = "... votre dernier(ère) conjoint(e) est décédé(e)"))
freq(BIEF$SEPARE)

## TRAVACT
freq(BIEF$TRAVACT)
BIEF <- BIEF %>% mutate(TRAVACT = recode(as.character(TRAVACT), "1" = "Oui", "2" = "Non"))
freq(BIEF$TRAVACT)

## DEJATRAV1
freq(BIEF$DEJATRAV1)
BIEF <- BIEF %>% mutate(DEJATRAV1 = recode(as.character(DEJATRAV1), "1" = "Oui", "2" = "Non"))
freq(BIEF$DEJATRAV1)

## DEJATRAV2
freq(BIEF$DEJATRAV2)
BIEF <- BIEF %>% mutate(DEJATRAV2 = recode(as.character(DEJATRAV2), "1" = "Oui", "2" = "Non"))
freq(BIEF$DEJATRAV2)

## COUPLE
freq(BIEF$COUPLE)
BIEF <- BIEF %>% mutate(COUPLE = recode(as.character(COUPLE), "1" = "Oui, avec quelqu'un qui vit avec vous dans ce logement", "2" = "Oui, avec quelqu'un qui vit dans un autre logement", "3" = "Non, mais vous avez déjà été en couple par le passé", "4" = "Non, vous n'avez jamais été en couple"))
freq(BIEF$COUPLE)

## PACS
freq(BIEF$PACS)
BIEF <- BIEF %>% mutate(PACS = recode(as.character(PACS), "1" = "Oui", "2" = "Non"))
freq(BIEF$PACS)

## MARI
freq(BIEF$MARI)
BIEF <- BIEF %>% mutate(MARI = recode(as.character(MARI), "1" = "Oui", "2" = "Non"))
freq(BIEF$MARI)

## AUT_UNION
freq(BIEF$AUT_UNION)
BIEF <- BIEF %>% mutate(AUT_UNION = recode(as.character(AUT_UNION), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_UNION)

## ENF
freq(BIEF$ENF)
BIEF <- BIEF %>% mutate(ENF = recode(as.character(ENF), "1" = "Oui", "2" = "Non"))
freq(BIEF$ENF)

## NBENF_ADOP_UN
freq(BIEF$NBENF_ADOP_UN)
BIEF <- BIEF %>% mutate(NBENF_ADOP_UN = recode(as.character(NBENF_ADOP_UN), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENF_ADOP_UN)

## NBENFLOG_UN
freq(BIEF$NBENFLOG_UN)
BIEF <- BIEF %>% mutate(NBENFLOG_UN = recode(as.character(NBENFLOG_UN), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFLOG_UN)

## NBENFLOG
freq(BIEF$NBENFLOG)
BIEF <- BIEF %>% mutate(NBENFLOG = recode(as.character(NBENFLOG), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFLOG)

## NBENFAIL_UN
freq(BIEF$NBENFAIL_UN)
BIEF <- BIEF %>% mutate(NBENFAIL_UN = recode(as.character(NBENFAIL_UN), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFAIL_UN)

## FINETU
freq(BIEF$FINETU)
BIEF <- BIEF %>% mutate(FINETU = recode(as.character(FINETU), "1" = "Oui", "2" = "Non"))
freq(BIEF$FINETU)

## VECU_PLACE
freq(BIEF$VECU_PLACE)
BIEF <- BIEF %>% mutate(VECU_PLACE = recode(as.character(VECU_PLACE), "1" = "Oui", "2" = "Non"))
freq(BIEF$VECU_PLACE)

## HEBERG
freq(BIEF$HEBERG)
BIEF <- BIEF %>% mutate(HEBERG = recode(as.character(HEBERG), "1" = "Oui", "2" = "Non"))
freq(BIEF$HEBERG)

## LANGUE2_ENFE
freq(BIEF$LANGUE2_ENFE)
BIEF <- BIEF %>% mutate(LANGUE2_ENFE = recode(as.character(LANGUE2_ENFE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE2_ENFE)

## LANGUE3_ENFE
freq(BIEF$LANGUE3_ENFE)
BIEF <- BIEF %>% mutate(LANGUE3_ENFE = recode(as.character(LANGUE3_ENFE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE3_ENFE)

## LANGUE4_ENFE
freq(BIEF$LANGUE4_ENFE)
BIEF <- BIEF %>% mutate(LANGUE4_ENFE = recode(as.character(LANGUE4_ENFE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE4_ENFE)

## LANGUE1_ENTOUE
freq(BIEF$LANGUE1_ENTOUE)
BIEF <- BIEF %>% mutate(LANGUE1_ENTOUE = recode(as.character(LANGUE1_ENTOUE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE1_ENTOUE)

## LANGUE2_ENTOUE
freq(BIEF$LANGUE2_ENTOUE)
BIEF <- BIEF %>% mutate(LANGUE2_ENTOUE = recode(as.character(LANGUE2_ENTOUE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE2_ENTOUE)

## LANGUE3_ENTOUE
freq(BIEF$LANGUE3_ENTOUE)
BIEF <- BIEF %>% mutate(LANGUE3_ENTOUE = recode(as.character(LANGUE3_ENTOUE), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE3_ENTOUE)

## SEXE_C
freq(BIEF$SEXE_C)
BIEF <- BIEF %>% mutate(SEXE_C = recode(as.character(SEXE_C), "1" = "Un homme", "2" = "Une femme"))
freq(BIEF$SEXE_C)

## LIEUNAIS_C
freq(BIEF$LIEUNAIS_C)
BIEF <- BIEF %>% mutate(LIEUNAIS_C = recode(as.character(LIEUNAIS_C), "1" = "Oui", "2" = "Non"))
freq(BIEF$LIEUNAIS_C)

## STATUT_C
freq(BIEF$STATUT_C)
BIEF <- BIEF %>% mutate(STATUT_C = recode(as.character(STATUT_C), "1" = "A son compte (y compris gérant(e) de société salarié(e))", "2" = "Salarié(e) de la fonction publique (Etat, territoriale, hospitalière)", "3" = "Salarié(e) d'une entreprise (y compris d'une association ou de la sécurité sociale)", "4" = "Salarié(e) d'un particulier", "5" = "Aide familial(e) non rémunéré(e)"))
freq(BIEF$STATUT_C)

## ENFAV_C
freq(BIEF$ENFAV_C)
BIEF <- BIEF %>% mutate(ENFAV_C = recode(as.character(ENFAV_C), "1" = "Oui", "2" = "Non"))
freq(BIEF$ENFAV_C)

## NBENFAV_VENU_C1
freq(BIEF$NBENFAV_VENU_C1)
BIEF <- BIEF %>% mutate(NBENFAV_VENU_C1 = recode(as.character(NBENFAV_VENU_C1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFAV_VENU_C1)

## ENF21_AUTPAR_C1
freq(BIEF$ENF21_AUTPAR_C1)
BIEF <- BIEF %>% mutate(ENF21_AUTPAR_C1 = recode(as.character(ENF21_AUTPAR_C1), "NA" = "Non - Non"))
freq(BIEF$ENF21_AUTPAR_C1)

## ENF21_AUTPAR_C2
freq(BIEF$ENF21_AUTPAR_C2)
BIEF <- BIEF %>% mutate(ENF21_AUTPAR_C2 = recode(as.character(ENF21_AUTPAR_C2), "NA" = "Non - Non"))
freq(BIEF$ENF21_AUTPAR_C2)

## ENF21_AUTPAR_C3
freq(BIEF$ENF21_AUTPAR_C3)
BIEF <- BIEF %>% mutate(ENF21_AUTPAR_C3 = recode(as.character(ENF21_AUTPAR_C3), "NA" = "Non - Non"))
freq(BIEF$ENF21_AUTPAR_C3)

## ENF21_AUTPAR_C4
freq(BIEF$ENF21_AUTPAR_C4)
BIEF <- BIEF %>% mutate(ENF21_AUTPAR_C4 = recode(as.character(ENF21_AUTPAR_C4), "NA" = "Non - Non"))
freq(BIEF$ENF21_AUTPAR_C4)

## PACS_U1
freq(BIEF$PACS_U1)
BIEF <- BIEF %>% mutate(PACS_U1 = recode(as.character(PACS_U1), "1" = "Oui", "2" = "Non"))
freq(BIEF$PACS_U1)

## MARI_U1
freq(BIEF$MARI_U1)
BIEF <- BIEF %>% mutate(MARI_U1 = recode(as.character(MARI_U1), "1" = "Oui", "2" = "Non"))
freq(BIEF$MARI_U1)

## SEPARE_U1
freq(BIEF$SEPARE_U1)
BIEF <- BIEF %>% mutate(SEPARE_U1 = recode(as.character(SEPARE_U1), "1" = "...vous vous êtes séparé(e)s", "2" = "... votre premier(ère) conjoint(e) est décédé(e)"))
freq(BIEF$SEPARE_U1)

## ENFAV_C_U1
freq(BIEF$ENFAV_C_U1)
BIEF <- BIEF %>% mutate(ENFAV_C_U1 = recode(as.character(ENFAV_C_U1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ENFAV_C_U1)

## NBENFAV_C1_VENU_U1
freq(BIEF$NBENFAV_C1_VENU_U1)
BIEF <- BIEF %>% mutate(NBENFAV_C1_VENU_U1 = recode(as.character(NBENFAV_C1_VENU_U1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFAV_C1_VENU_U1)

## PACS_U2
freq(BIEF$PACS_U2)
BIEF <- BIEF %>% mutate(PACS_U2 = recode(as.character(PACS_U2), "1" = "Oui", "2" = "Non"))
freq(BIEF$PACS_U2)

## MARI_U2
freq(BIEF$MARI_U2)
BIEF <- BIEF %>% mutate(MARI_U2 = recode(as.character(MARI_U2), "1" = "Oui", "2" = "Non"))
freq(BIEF$MARI_U2)

## SEPARE_U2
freq(BIEF$SEPARE_U2)
BIEF <- BIEF %>% mutate(SEPARE_U2 = recode(as.character(SEPARE_U2), "1" = "...vous vous êtes séparé(e)s", "2" = "... votre premier(ère) conjoint(e) est décédé(e)"))
freq(BIEF$SEPARE_U2)

## ENFAV_C_U2
freq(BIEF$ENFAV_C_U2)
BIEF <- BIEF %>% mutate(ENFAV_C_U2 = recode(as.character(ENFAV_C_U2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ENFAV_C_U2)

## NBENFAV_C1_VENU_U2
freq(BIEF$NBENFAV_C1_VENU_U2)
BIEF <- BIEF %>% mutate(NBENFAV_C1_VENU_U2 = recode(as.character(NBENFAV_C1_VENU_U2), "1" = "Oui", "2" = "Non"))
freq(BIEF$NBENFAV_C1_VENU_U2)

## SEXE_ENFLOG1
freq(BIEF$SEXE_ENFLOG1)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG1 = recode(as.character(SEXE_ENFLOG1), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG1)

## NEFRANCE_ENFLOG1
freq(BIEF$NEFRANCE_ENFLOG1)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG1 = recode(as.character(NEFRANCE_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG1)

## PARENT_VIT_ENFLOG1
freq(BIEF$PARENT_VIT_ENFLOG1)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG1 = recode(as.character(PARENT_VIT_ENFLOG1), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG1)

## PARENT_FR_ENFLOG1
freq(BIEF$PARENT_FR_ENFLOG1)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG1 = recode(as.character(PARENT_FR_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG1)

## PARENT_FREQ_ENFLOG1
freq(BIEF$PARENT_FREQ_ENFLOG1)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG1 = recode(as.character(PARENT_FREQ_ENFLOG1), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG1)

## PARENT_DORT_ENFLOG1
freq(BIEF$PARENT_DORT_ENFLOG1)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG1 = recode(as.character(PARENT_DORT_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG1)

## PARENT_VECU_ENFLOG1
freq(BIEF$PARENT_VECU_ENFLOG1)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG1 = recode(as.character(PARENT_VECU_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG1)

## SEP_AUT_PAR_ENFLOG1
freq(BIEF$SEP_AUT_PAR_ENFLOG1)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG1 = recode(as.character(SEP_AUT_PAR_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG1)

## RESID_ENFLOG1
freq(BIEF$RESID_ENFLOG1)
BIEF <- BIEF %>% mutate(RESID_ENFLOG1 = recode(as.character(RESID_ENFLOG1), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG1)

## SANTE_ENFLOG1
freq(BIEF$SANTE_ENFLOG1)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG1 = recode(as.character(SANTE_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG1)

## ASE_ENFLOG1
freq(BIEF$ASE_ENFLOG1)
BIEF <- BIEF %>% mutate(ASE_ENFLOG1 = recode(as.character(ASE_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG1)

## ADOPT_ENFLOG1
freq(BIEF$ADOPT_ENFLOG1)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG1 = recode(as.character(ADOPT_ENFLOG1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG1)

## SEXE_ENFLOG2
freq(BIEF$SEXE_ENFLOG2)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG2 = recode(as.character(SEXE_ENFLOG2), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG2)

## NEFRANCE_ENFLOG2
freq(BIEF$NEFRANCE_ENFLOG2)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG2 = recode(as.character(NEFRANCE_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG2)

## PARENT_VIT_ENFLOG2
freq(BIEF$PARENT_VIT_ENFLOG2)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG2 = recode(as.character(PARENT_VIT_ENFLOG2), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG2)

## PARENT_FR_ENFLOG2
freq(BIEF$PARENT_FR_ENFLOG2)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG2 = recode(as.character(PARENT_FR_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG2)

## PARENT_FREQ_ENFLOG2
freq(BIEF$PARENT_FREQ_ENFLOG2)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG2 = recode(as.character(PARENT_FREQ_ENFLOG2), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG2)

## PARENT_DORT_ENFLOG2
freq(BIEF$PARENT_DORT_ENFLOG2)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG2 = recode(as.character(PARENT_DORT_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG2)

## PARENT_VECU_ENFLOG2
freq(BIEF$PARENT_VECU_ENFLOG2)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG2 = recode(as.character(PARENT_VECU_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG2)

## SEP_AUT_PAR_ENFLOG2
freq(BIEF$SEP_AUT_PAR_ENFLOG2)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG2 = recode(as.character(SEP_AUT_PAR_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG2)

## RESID_ENFLOG2
freq(BIEF$RESID_ENFLOG2)
BIEF <- BIEF %>% mutate(RESID_ENFLOG2 = recode(as.character(RESID_ENFLOG2), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG2)

## SANTE_ENFLOG2
freq(BIEF$SANTE_ENFLOG2)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG2 = recode(as.character(SANTE_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG2)

## ASE_ENFLOG2
freq(BIEF$ASE_ENFLOG2)
BIEF <- BIEF %>% mutate(ASE_ENFLOG2 = recode(as.character(ASE_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG2)

## ADOPT_ENFLOG2
freq(BIEF$ADOPT_ENFLOG2)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG2 = recode(as.character(ADOPT_ENFLOG2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG2)

## SEXE_ENFLOG3
freq(BIEF$SEXE_ENFLOG3)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG3 = recode(as.character(SEXE_ENFLOG3), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG3)

## NEFRANCE_ENFLOG3
freq(BIEF$NEFRANCE_ENFLOG3)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG3 = recode(as.character(NEFRANCE_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG3)

## PARENT_VIT_ENFLOG3
freq(BIEF$PARENT_VIT_ENFLOG3)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG3 = recode(as.character(PARENT_VIT_ENFLOG3), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG3)

## PARENT_FR_ENFLOG3
freq(BIEF$PARENT_FR_ENFLOG3)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG3 = recode(as.character(PARENT_FR_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG3)

## PARENT_FREQ_ENFLOG3
freq(BIEF$PARENT_FREQ_ENFLOG3)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG3 = recode(as.character(PARENT_FREQ_ENFLOG3), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG3)

## PARENT_DORT_ENFLOG3
freq(BIEF$PARENT_DORT_ENFLOG3)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG3 = recode(as.character(PARENT_DORT_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG3)

## PARENT_VECU_ENFLOG3
freq(BIEF$PARENT_VECU_ENFLOG3)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG3 = recode(as.character(PARENT_VECU_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG3)

## SEP_AUT_PAR_ENFLOG3
freq(BIEF$SEP_AUT_PAR_ENFLOG3)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG3 = recode(as.character(SEP_AUT_PAR_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG3)

## RESID_ENFLOG3
freq(BIEF$RESID_ENFLOG3)
BIEF <- BIEF %>% mutate(RESID_ENFLOG3 = recode(as.character(RESID_ENFLOG3), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG3)

## SANTE_ENFLOG3
freq(BIEF$SANTE_ENFLOG3)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG3 = recode(as.character(SANTE_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG3)

## ASE_ENFLOG3
freq(BIEF$ASE_ENFLOG3)
BIEF <- BIEF %>% mutate(ASE_ENFLOG3 = recode(as.character(ASE_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG3)

## ADOPT_ENFLOG3
freq(BIEF$ADOPT_ENFLOG3)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG3 = recode(as.character(ADOPT_ENFLOG3), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG3)

## SEXE_ENFLOG4
freq(BIEF$SEXE_ENFLOG4)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG4 = recode(as.character(SEXE_ENFLOG4), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG4)

## NEFRANCE_ENFLOG4
freq(BIEF$NEFRANCE_ENFLOG4)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG4 = recode(as.character(NEFRANCE_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG4)

## PARENT_VIT_ENFLOG4
freq(BIEF$PARENT_VIT_ENFLOG4)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG4 = recode(as.character(PARENT_VIT_ENFLOG4), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG4)

## PARENT_FR_ENFLOG4
freq(BIEF$PARENT_FR_ENFLOG4)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG4 = recode(as.character(PARENT_FR_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG4)

## PARENT_FREQ_ENFLOG4
freq(BIEF$PARENT_FREQ_ENFLOG4)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG4 = recode(as.character(PARENT_FREQ_ENFLOG4), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG4)

## PARENT_DORT_ENFLOG4
freq(BIEF$PARENT_DORT_ENFLOG4)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG4 = recode(as.character(PARENT_DORT_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG4)

## PARENT_VECU_ENFLOG4
freq(BIEF$PARENT_VECU_ENFLOG4)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG4 = recode(as.character(PARENT_VECU_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG4)

## SEP_AUT_PAR_ENFLOG4
freq(BIEF$SEP_AUT_PAR_ENFLOG4)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG4 = recode(as.character(SEP_AUT_PAR_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG4)

## RESID_ENFLOG4
freq(BIEF$RESID_ENFLOG4)
BIEF <- BIEF %>% mutate(RESID_ENFLOG4 = recode(as.character(RESID_ENFLOG4), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG4)

## SANTE_ENFLOG4
freq(BIEF$SANTE_ENFLOG4)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG4 = recode(as.character(SANTE_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG4)

## ASE_ENFLOG4
freq(BIEF$ASE_ENFLOG4)
BIEF <- BIEF %>% mutate(ASE_ENFLOG4 = recode(as.character(ASE_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG4)

## ADOPT_ENFLOG4
freq(BIEF$ADOPT_ENFLOG4)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG4 = recode(as.character(ADOPT_ENFLOG4), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG4)

## SEXE_ENFLOG5
freq(BIEF$SEXE_ENFLOG5)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG5 = recode(as.character(SEXE_ENFLOG5), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG5)

## NEFRANCE_ENFLOG5
freq(BIEF$NEFRANCE_ENFLOG5)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG5 = recode(as.character(NEFRANCE_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG5)

## PARENT_VIT_ENFLOG5
freq(BIEF$PARENT_VIT_ENFLOG5)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG5 = recode(as.character(PARENT_VIT_ENFLOG5), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG5)

## PARENT_FR_ENFLOG5
freq(BIEF$PARENT_FR_ENFLOG5)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG5 = recode(as.character(PARENT_FR_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG5)

## PARENT_FREQ_ENFLOG5
freq(BIEF$PARENT_FREQ_ENFLOG5)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG5 = recode(as.character(PARENT_FREQ_ENFLOG5), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG5)

## PARENT_DORT_ENFLOG5
freq(BIEF$PARENT_DORT_ENFLOG5)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG5 = recode(as.character(PARENT_DORT_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG5)

## PARENT_VECU_ENFLOG5
freq(BIEF$PARENT_VECU_ENFLOG5)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG5 = recode(as.character(PARENT_VECU_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG5)

## SEP_AUT_PAR_ENFLOG5
freq(BIEF$SEP_AUT_PAR_ENFLOG5)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG5 = recode(as.character(SEP_AUT_PAR_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG5)

## RESID_ENFLOG5
freq(BIEF$RESID_ENFLOG5)
BIEF <- BIEF %>% mutate(RESID_ENFLOG5 = recode(as.character(RESID_ENFLOG5), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG5)

## SANTE_ENFLOG5
freq(BIEF$SANTE_ENFLOG5)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG5 = recode(as.character(SANTE_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG5)

## ASE_ENFLOG5
freq(BIEF$ASE_ENFLOG5)
BIEF <- BIEF %>% mutate(ASE_ENFLOG5 = recode(as.character(ASE_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG5)

## ADOPT_ENFLOG5
freq(BIEF$ADOPT_ENFLOG5)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG5 = recode(as.character(ADOPT_ENFLOG5), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG5)

## SEXE_ENFLOG6
freq(BIEF$SEXE_ENFLOG6)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG6 = recode(as.character(SEXE_ENFLOG6), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG6)

## NEFRANCE_ENFLOG6
freq(BIEF$NEFRANCE_ENFLOG6)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG6 = recode(as.character(NEFRANCE_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG6)

## PARENT_VIT_ENFLOG6
freq(BIEF$PARENT_VIT_ENFLOG6)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG6 = recode(as.character(PARENT_VIT_ENFLOG6), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG6)

## PARENT_FR_ENFLOG6
freq(BIEF$PARENT_FR_ENFLOG6)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG6 = recode(as.character(PARENT_FR_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG6)

## PARENT_FREQ_ENFLOG6
freq(BIEF$PARENT_FREQ_ENFLOG6)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG6 = recode(as.character(PARENT_FREQ_ENFLOG6), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG6)

## PARENT_DORT_ENFLOG6
freq(BIEF$PARENT_DORT_ENFLOG6)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG6 = recode(as.character(PARENT_DORT_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG6)

## PARENT_VECU_ENFLOG6
freq(BIEF$PARENT_VECU_ENFLOG6)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG6 = recode(as.character(PARENT_VECU_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG6)

## SEP_AUT_PAR_ENFLOG6
freq(BIEF$SEP_AUT_PAR_ENFLOG6)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG6 = recode(as.character(SEP_AUT_PAR_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG6)

## RESID_ENFLOG6
freq(BIEF$RESID_ENFLOG6)
BIEF <- BIEF %>% mutate(RESID_ENFLOG6 = recode(as.character(RESID_ENFLOG6), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG6)

## SANTE_ENFLOG6
freq(BIEF$SANTE_ENFLOG6)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG6 = recode(as.character(SANTE_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG6)

## ASE_ENFLOG6
freq(BIEF$ASE_ENFLOG6)
BIEF <- BIEF %>% mutate(ASE_ENFLOG6 = recode(as.character(ASE_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG6)

## ADOPT_ENFLOG6
freq(BIEF$ADOPT_ENFLOG6)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG6 = recode(as.character(ADOPT_ENFLOG6), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG6)

## SEXE_ENFLOG7
freq(BIEF$SEXE_ENFLOG7)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG7 = recode(as.character(SEXE_ENFLOG7), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG7)

## NEFRANCE_ENFLOG7
freq(BIEF$NEFRANCE_ENFLOG7)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG7 = recode(as.character(NEFRANCE_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG7)

## PARENT_VIT_ENFLOG7
freq(BIEF$PARENT_VIT_ENFLOG7)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG7 = recode(as.character(PARENT_VIT_ENFLOG7), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG7)

## PARENT_FR_ENFLOG7
freq(BIEF$PARENT_FR_ENFLOG7)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG7 = recode(as.character(PARENT_FR_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG7)

## PARENT_FREQ_ENFLOG7
freq(BIEF$PARENT_FREQ_ENFLOG7)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG7 = recode(as.character(PARENT_FREQ_ENFLOG7), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG7)

## PARENT_DORT_ENFLOG7
freq(BIEF$PARENT_DORT_ENFLOG7)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG7 = recode(as.character(PARENT_DORT_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG7)

## PARENT_VECU_ENFLOG7
freq(BIEF$PARENT_VECU_ENFLOG7)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG7 = recode(as.character(PARENT_VECU_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG7)

## SEP_AUT_PAR_ENFLOG7
freq(BIEF$SEP_AUT_PAR_ENFLOG7)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG7 = recode(as.character(SEP_AUT_PAR_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG7)

## RESID_ENFLOG7
freq(BIEF$RESID_ENFLOG7)
BIEF <- BIEF %>% mutate(RESID_ENFLOG7 = recode(as.character(RESID_ENFLOG7), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG7)

## SANTE_ENFLOG7
freq(BIEF$SANTE_ENFLOG7)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG7 = recode(as.character(SANTE_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG7)

## ASE_ENFLOG7
freq(BIEF$ASE_ENFLOG7)
BIEF <- BIEF %>% mutate(ASE_ENFLOG7 = recode(as.character(ASE_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG7)

## ADOPT_ENFLOG7
freq(BIEF$ADOPT_ENFLOG7)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG7 = recode(as.character(ADOPT_ENFLOG7), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG7)

## SEXE_ENFLOG8
freq(BIEF$SEXE_ENFLOG8)
BIEF <- BIEF %>% mutate(SEXE_ENFLOG8 = recode(as.character(SEXE_ENFLOG8), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFLOG8)

## NEFRANCE_ENFLOG8
freq(BIEF$NEFRANCE_ENFLOG8)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFLOG8 = recode(as.character(NEFRANCE_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFLOG8)

## PARENT_VIT_ENFLOG8
freq(BIEF$PARENT_VIT_ENFLOG8)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFLOG8 = recode(as.character(PARENT_VIT_ENFLOG8), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFLOG8)

## PARENT_FR_ENFLOG8
freq(BIEF$PARENT_FR_ENFLOG8)
BIEF <- BIEF %>% mutate(PARENT_FR_ENFLOG8 = recode(as.character(PARENT_FR_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_FR_ENFLOG8)

## PARENT_FREQ_ENFLOG8
freq(BIEF$PARENT_FREQ_ENFLOG8)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFLOG8 = recode(as.character(PARENT_FREQ_ENFLOG8), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFLOG8)

## PARENT_DORT_ENFLOG8
freq(BIEF$PARENT_DORT_ENFLOG8)
BIEF <- BIEF %>% mutate(PARENT_DORT_ENFLOG8 = recode(as.character(PARENT_DORT_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_DORT_ENFLOG8)

## PARENT_VECU_ENFLOG8
freq(BIEF$PARENT_VECU_ENFLOG8)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFLOG8 = recode(as.character(PARENT_VECU_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFLOG8)

## SEP_AUT_PAR_ENFLOG8
freq(BIEF$SEP_AUT_PAR_ENFLOG8)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFLOG8 = recode(as.character(SEP_AUT_PAR_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFLOG8)

## RESID_ENFLOG8
freq(BIEF$RESID_ENFLOG8)
BIEF <- BIEF %>% mutate(RESID_ENFLOG8 = recode(as.character(RESID_ENFLOG8), "2" = "Pas de décision de justice", "3" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "4" = "Principalement chez vous", "5" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFLOG8)

## SANTE_ENFLOG8
freq(BIEF$SANTE_ENFLOG8)
BIEF <- BIEF %>% mutate(SANTE_ENFLOG8 = recode(as.character(SANTE_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFLOG8)

## ASE_ENFLOG8
freq(BIEF$ASE_ENFLOG8)
BIEF <- BIEF %>% mutate(ASE_ENFLOG8 = recode(as.character(ASE_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFLOG8)

## ADOPT_ENFLOG8
freq(BIEF$ADOPT_ENFLOG8)
BIEF <- BIEF %>% mutate(ADOPT_ENFLOG8 = recode(as.character(ADOPT_ENFLOG8), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFLOG8)

## SEXE_ENFAIL1
freq(BIEF$SEXE_ENFAIL1)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL1 = recode(as.character(SEXE_ENFAIL1), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL1)

## NEFRANCE_ENFAIL1
freq(BIEF$NEFRANCE_ENFAIL1)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL1 = recode(as.character(NEFRANCE_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL1)

## PARENT_VIT_ENFAIL1
freq(BIEF$PARENT_VIT_ENFAIL1)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL1 = recode(as.character(PARENT_VIT_ENFAIL1), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL1)

## PARENT_VECU_ENFAIL1
freq(BIEF$PARENT_VECU_ENFAIL1)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL1 = recode(as.character(PARENT_VECU_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL1)

## DC_ENFAIL1
freq(BIEF$DC_ENFAIL1)
BIEF <- BIEF %>% mutate(DC_ENFAIL1 = recode(as.character(DC_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL1)

## PARENT_FREQ_ENFAIL1
freq(BIEF$PARENT_FREQ_ENFAIL1)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL1 = recode(as.character(PARENT_FREQ_ENFAIL1), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL1)

## FR_ENFAIL1
freq(BIEF$FR_ENFAIL1)
BIEF <- BIEF %>% mutate(FR_ENFAIL1 = recode(as.character(FR_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL1)

## LOG_ENFAIL1
freq(BIEF$LOG_ENFAIL1)
BIEF <- BIEF %>% mutate(LOG_ENFAIL1 = recode(as.character(LOG_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL1)

## AUT_PAR_ENFAIL1
freq(BIEF$AUT_PAR_ENFAIL1)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL1 = recode(as.character(AUT_PAR_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL1)

## DORT_ENFAIL1
freq(BIEF$DORT_ENFAIL1)
BIEF <- BIEF %>% mutate(DORT_ENFAIL1 = recode(as.character(DORT_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL1)

## SEP_AUT_PAR_ENFAIL1
freq(BIEF$SEP_AUT_PAR_ENFAIL1)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL1 = recode(as.character(SEP_AUT_PAR_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL1)

## RESID_ENFAIL1
freq(BIEF$RESID_ENFAIL1)
BIEF <- BIEF %>% mutate(RESID_ENFAIL1 = recode(as.character(RESID_ENFAIL1), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL1)

## SANTE_ENFAIL1
freq(BIEF$SANTE_ENFAIL1)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL1 = recode(as.character(SANTE_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL1)

## ASE_ENFAIL1
freq(BIEF$ASE_ENFAIL1)
BIEF <- BIEF %>% mutate(ASE_ENFAIL1 = recode(as.character(ASE_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL1)

## ADOPT_ENFAIL1
freq(BIEF$ADOPT_ENFAIL1)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL1 = recode(as.character(ADOPT_ENFAIL1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL1)

## SEXE_ENFAIL2
freq(BIEF$SEXE_ENFAIL2)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL2 = recode(as.character(SEXE_ENFAIL2), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL2)

## NEFRANCE_ENFAIL2
freq(BIEF$NEFRANCE_ENFAIL2)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL2 = recode(as.character(NEFRANCE_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL2)

## PARENT_VIT_ENFAIL2
freq(BIEF$PARENT_VIT_ENFAIL2)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL2 = recode(as.character(PARENT_VIT_ENFAIL2), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL2)

## PARENT_VECU_ENFAIL2
freq(BIEF$PARENT_VECU_ENFAIL2)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL2 = recode(as.character(PARENT_VECU_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL2)

## DC_ENFAIL2
freq(BIEF$DC_ENFAIL2)
BIEF <- BIEF %>% mutate(DC_ENFAIL2 = recode(as.character(DC_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL2)

## PARENT_FREQ_ENFAIL2
freq(BIEF$PARENT_FREQ_ENFAIL2)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL2 = recode(as.character(PARENT_FREQ_ENFAIL2), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL2)

## FR_ENFAIL2
freq(BIEF$FR_ENFAIL2)
BIEF <- BIEF %>% mutate(FR_ENFAIL2 = recode(as.character(FR_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL2)

## LOG_ENFAIL2
freq(BIEF$LOG_ENFAIL2)
BIEF <- BIEF %>% mutate(LOG_ENFAIL2 = recode(as.character(LOG_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL2)

## AUT_PAR_ENFAIL2
freq(BIEF$AUT_PAR_ENFAIL2)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL2 = recode(as.character(AUT_PAR_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL2)

## DORT_ENFAIL2
freq(BIEF$DORT_ENFAIL2)
BIEF <- BIEF %>% mutate(DORT_ENFAIL2 = recode(as.character(DORT_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL2)

## SEP_AUT_PAR_ENFAIL2
freq(BIEF$SEP_AUT_PAR_ENFAIL2)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL2 = recode(as.character(SEP_AUT_PAR_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL2)

## RESID_ENFAIL2
freq(BIEF$RESID_ENFAIL2)
BIEF <- BIEF %>% mutate(RESID_ENFAIL2 = recode(as.character(RESID_ENFAIL2), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL2)

## SANTE_ENFAIL2
freq(BIEF$SANTE_ENFAIL2)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL2 = recode(as.character(SANTE_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL2)

## ASE_ENFAIL2
freq(BIEF$ASE_ENFAIL2)
BIEF <- BIEF %>% mutate(ASE_ENFAIL2 = recode(as.character(ASE_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL2)

## ADOPT_ENFAIL2
freq(BIEF$ADOPT_ENFAIL2)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL2 = recode(as.character(ADOPT_ENFAIL2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL2)

## SEXE_ENFAIL3
freq(BIEF$SEXE_ENFAIL3)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL3 = recode(as.character(SEXE_ENFAIL3), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL3)

## NEFRANCE_ENFAIL3
freq(BIEF$NEFRANCE_ENFAIL3)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL3 = recode(as.character(NEFRANCE_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL3)

## PARENT_VIT_ENFAIL3
freq(BIEF$PARENT_VIT_ENFAIL3)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL3 = recode(as.character(PARENT_VIT_ENFAIL3), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL3)

## PARENT_VECU_ENFAIL3
freq(BIEF$PARENT_VECU_ENFAIL3)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL3 = recode(as.character(PARENT_VECU_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL3)

## DC_ENFAIL3
freq(BIEF$DC_ENFAIL3)
BIEF <- BIEF %>% mutate(DC_ENFAIL3 = recode(as.character(DC_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL3)

## PARENT_FREQ_ENFAIL3
freq(BIEF$PARENT_FREQ_ENFAIL3)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL3 = recode(as.character(PARENT_FREQ_ENFAIL3), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL3)

## FR_ENFAIL3
freq(BIEF$FR_ENFAIL3)
BIEF <- BIEF %>% mutate(FR_ENFAIL3 = recode(as.character(FR_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL3)

## LOG_ENFAIL3
freq(BIEF$LOG_ENFAIL3)
BIEF <- BIEF %>% mutate(LOG_ENFAIL3 = recode(as.character(LOG_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL3)

## AUT_PAR_ENFAIL3
freq(BIEF$AUT_PAR_ENFAIL3)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL3 = recode(as.character(AUT_PAR_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL3)

## DORT_ENFAIL3
freq(BIEF$DORT_ENFAIL3)
BIEF <- BIEF %>% mutate(DORT_ENFAIL3 = recode(as.character(DORT_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL3)

## SEP_AUT_PAR_ENFAIL3
freq(BIEF$SEP_AUT_PAR_ENFAIL3)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL3 = recode(as.character(SEP_AUT_PAR_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL3)

## RESID_ENFAIL3
freq(BIEF$RESID_ENFAIL3)
BIEF <- BIEF %>% mutate(RESID_ENFAIL3 = recode(as.character(RESID_ENFAIL3), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL3)

## SANTE_ENFAIL3
freq(BIEF$SANTE_ENFAIL3)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL3 = recode(as.character(SANTE_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL3)

## ASE_ENFAIL3
freq(BIEF$ASE_ENFAIL3)
BIEF <- BIEF %>% mutate(ASE_ENFAIL3 = recode(as.character(ASE_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL3)

## ADOPT_ENFAIL3
freq(BIEF$ADOPT_ENFAIL3)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL3 = recode(as.character(ADOPT_ENFAIL3), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL3)

## SEXE_ENFAIL4
freq(BIEF$SEXE_ENFAIL4)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL4 = recode(as.character(SEXE_ENFAIL4), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL4)

## NEFRANCE_ENFAIL4
freq(BIEF$NEFRANCE_ENFAIL4)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL4 = recode(as.character(NEFRANCE_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL4)

## PARENT_VIT_ENFAIL4
freq(BIEF$PARENT_VIT_ENFAIL4)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL4 = recode(as.character(PARENT_VIT_ENFAIL4), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL4)

## PARENT_VECU_ENFAIL4
freq(BIEF$PARENT_VECU_ENFAIL4)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL4 = recode(as.character(PARENT_VECU_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL4)

## DC_ENFAIL4
freq(BIEF$DC_ENFAIL4)
BIEF <- BIEF %>% mutate(DC_ENFAIL4 = recode(as.character(DC_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL4)

## PARENT_FREQ_ENFAIL4
freq(BIEF$PARENT_FREQ_ENFAIL4)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL4 = recode(as.character(PARENT_FREQ_ENFAIL4), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL4)

## FR_ENFAIL4
freq(BIEF$FR_ENFAIL4)
BIEF <- BIEF %>% mutate(FR_ENFAIL4 = recode(as.character(FR_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL4)

## LOG_ENFAIL4
freq(BIEF$LOG_ENFAIL4)
BIEF <- BIEF %>% mutate(LOG_ENFAIL4 = recode(as.character(LOG_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL4)

## AUT_PAR_ENFAIL4
freq(BIEF$AUT_PAR_ENFAIL4)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL4 = recode(as.character(AUT_PAR_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL4)

## DORT_ENFAIL4
freq(BIEF$DORT_ENFAIL4)
BIEF <- BIEF %>% mutate(DORT_ENFAIL4 = recode(as.character(DORT_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL4)

## SEP_AUT_PAR_ENFAIL4
freq(BIEF$SEP_AUT_PAR_ENFAIL4)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL4 = recode(as.character(SEP_AUT_PAR_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL4)

## RESID_ENFAIL4
freq(BIEF$RESID_ENFAIL4)
BIEF <- BIEF %>% mutate(RESID_ENFAIL4 = recode(as.character(RESID_ENFAIL4), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL4)

## SANTE_ENFAIL4
freq(BIEF$SANTE_ENFAIL4)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL4 = recode(as.character(SANTE_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL4)

## ASE_ENFAIL4
freq(BIEF$ASE_ENFAIL4)
BIEF <- BIEF %>% mutate(ASE_ENFAIL4 = recode(as.character(ASE_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL4)

## ADOPT_ENFAIL4
freq(BIEF$ADOPT_ENFAIL4)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL4 = recode(as.character(ADOPT_ENFAIL4), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL4)

## SEXE_ENFAIL5
freq(BIEF$SEXE_ENFAIL5)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL5 = recode(as.character(SEXE_ENFAIL5), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL5)

## NEFRANCE_ENFAIL5
freq(BIEF$NEFRANCE_ENFAIL5)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL5 = recode(as.character(NEFRANCE_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL5)

## PARENT_VIT_ENFAIL5
freq(BIEF$PARENT_VIT_ENFAIL5)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL5 = recode(as.character(PARENT_VIT_ENFAIL5), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL5)

## PARENT_VECU_ENFAIL5
freq(BIEF$PARENT_VECU_ENFAIL5)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL5 = recode(as.character(PARENT_VECU_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL5)

## DC_ENFAIL5
freq(BIEF$DC_ENFAIL5)
BIEF <- BIEF %>% mutate(DC_ENFAIL5 = recode(as.character(DC_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL5)

## PARENT_FREQ_ENFAIL5
freq(BIEF$PARENT_FREQ_ENFAIL5)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL5 = recode(as.character(PARENT_FREQ_ENFAIL5), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL5)

## FR_ENFAIL5
freq(BIEF$FR_ENFAIL5)
BIEF <- BIEF %>% mutate(FR_ENFAIL5 = recode(as.character(FR_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL5)

## LOG_ENFAIL5
freq(BIEF$LOG_ENFAIL5)
BIEF <- BIEF %>% mutate(LOG_ENFAIL5 = recode(as.character(LOG_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL5)

## AUT_PAR_ENFAIL5
freq(BIEF$AUT_PAR_ENFAIL5)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL5 = recode(as.character(AUT_PAR_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL5)

## DORT_ENFAIL5
freq(BIEF$DORT_ENFAIL5)
BIEF <- BIEF %>% mutate(DORT_ENFAIL5 = recode(as.character(DORT_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL5)

## SEP_AUT_PAR_ENFAIL5
freq(BIEF$SEP_AUT_PAR_ENFAIL5)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL5 = recode(as.character(SEP_AUT_PAR_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL5)

## RESID_ENFAIL5
freq(BIEF$RESID_ENFAIL5)
BIEF <- BIEF %>% mutate(RESID_ENFAIL5 = recode(as.character(RESID_ENFAIL5), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL5)

## SANTE_ENFAIL5
freq(BIEF$SANTE_ENFAIL5)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL5 = recode(as.character(SANTE_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL5)

## ASE_ENFAIL5
freq(BIEF$ASE_ENFAIL5)
BIEF <- BIEF %>% mutate(ASE_ENFAIL5 = recode(as.character(ASE_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL5)

## ADOPT_ENFAIL5
freq(BIEF$ADOPT_ENFAIL5)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL5 = recode(as.character(ADOPT_ENFAIL5), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL5)

## SEXE_ENFAIL6
freq(BIEF$SEXE_ENFAIL6)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL6 = recode(as.character(SEXE_ENFAIL6), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL6)

## NEFRANCE_ENFAIL6
freq(BIEF$NEFRANCE_ENFAIL6)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL6 = recode(as.character(NEFRANCE_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL6)

## PARENT_VIT_ENFAIL6
freq(BIEF$PARENT_VIT_ENFAIL6)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL6 = recode(as.character(PARENT_VIT_ENFAIL6), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL6)

## PARENT_VECU_ENFAIL6
freq(BIEF$PARENT_VECU_ENFAIL6)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL6 = recode(as.character(PARENT_VECU_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL6)

## DC_ENFAIL6
freq(BIEF$DC_ENFAIL6)
BIEF <- BIEF %>% mutate(DC_ENFAIL6 = recode(as.character(DC_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL6)

## PARENT_FREQ_ENFAIL6
freq(BIEF$PARENT_FREQ_ENFAIL6)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL6 = recode(as.character(PARENT_FREQ_ENFAIL6), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL6)

## FR_ENFAIL6
freq(BIEF$FR_ENFAIL6)
BIEF <- BIEF %>% mutate(FR_ENFAIL6 = recode(as.character(FR_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL6)

## LOG_ENFAIL6
freq(BIEF$LOG_ENFAIL6)
BIEF <- BIEF %>% mutate(LOG_ENFAIL6 = recode(as.character(LOG_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL6)

## AUT_PAR_ENFAIL6
freq(BIEF$AUT_PAR_ENFAIL6)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL6 = recode(as.character(AUT_PAR_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL6)

## DORT_ENFAIL6
freq(BIEF$DORT_ENFAIL6)
BIEF <- BIEF %>% mutate(DORT_ENFAIL6 = recode(as.character(DORT_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL6)

## SEP_AUT_PAR_ENFAIL6
freq(BIEF$SEP_AUT_PAR_ENFAIL6)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL6 = recode(as.character(SEP_AUT_PAR_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL6)

## RESID_ENFAIL6
freq(BIEF$RESID_ENFAIL6)
BIEF <- BIEF %>% mutate(RESID_ENFAIL6 = recode(as.character(RESID_ENFAIL6), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL6)

## SANTE_ENFAIL6
freq(BIEF$SANTE_ENFAIL6)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL6 = recode(as.character(SANTE_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL6)

## ASE_ENFAIL6
freq(BIEF$ASE_ENFAIL6)
BIEF <- BIEF %>% mutate(ASE_ENFAIL6 = recode(as.character(ASE_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL6)

## ADOPT_ENFAIL6
freq(BIEF$ADOPT_ENFAIL6)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL6 = recode(as.character(ADOPT_ENFAIL6), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL6)

## SEXE_ENFAIL7
freq(BIEF$SEXE_ENFAIL7)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL7 = recode(as.character(SEXE_ENFAIL7), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL7)

## NEFRANCE_ENFAIL7
freq(BIEF$NEFRANCE_ENFAIL7)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL7 = recode(as.character(NEFRANCE_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL7)

## PARENT_VIT_ENFAIL7
freq(BIEF$PARENT_VIT_ENFAIL7)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL7 = recode(as.character(PARENT_VIT_ENFAIL7), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL7)

## PARENT_VECU_ENFAIL7
freq(BIEF$PARENT_VECU_ENFAIL7)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL7 = recode(as.character(PARENT_VECU_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL7)

## DC_ENFAIL7
freq(BIEF$DC_ENFAIL7)
BIEF <- BIEF %>% mutate(DC_ENFAIL7 = recode(as.character(DC_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL7)

## PARENT_FREQ_ENFAIL7
freq(BIEF$PARENT_FREQ_ENFAIL7)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL7 = recode(as.character(PARENT_FREQ_ENFAIL7), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL7)

## FR_ENFAIL7
freq(BIEF$FR_ENFAIL7)
BIEF <- BIEF %>% mutate(FR_ENFAIL7 = recode(as.character(FR_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL7)

## LOG_ENFAIL7
freq(BIEF$LOG_ENFAIL7)
BIEF <- BIEF %>% mutate(LOG_ENFAIL7 = recode(as.character(LOG_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL7)

## AUT_PAR_ENFAIL7
freq(BIEF$AUT_PAR_ENFAIL7)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL7 = recode(as.character(AUT_PAR_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL7)

## DORT_ENFAIL7
freq(BIEF$DORT_ENFAIL7)
BIEF <- BIEF %>% mutate(DORT_ENFAIL7 = recode(as.character(DORT_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL7)

## SEP_AUT_PAR_ENFAIL7
freq(BIEF$SEP_AUT_PAR_ENFAIL7)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL7 = recode(as.character(SEP_AUT_PAR_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL7)

## RESID_ENFAIL7
freq(BIEF$RESID_ENFAIL7)
BIEF <- BIEF %>% mutate(RESID_ENFAIL7 = recode(as.character(RESID_ENFAIL7), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL7)

## SANTE_ENFAIL7
freq(BIEF$SANTE_ENFAIL7)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL7 = recode(as.character(SANTE_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL7)

## ASE_ENFAIL7
freq(BIEF$ASE_ENFAIL7)
BIEF <- BIEF %>% mutate(ASE_ENFAIL7 = recode(as.character(ASE_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL7)

## ADOPT_ENFAIL7
freq(BIEF$ADOPT_ENFAIL7)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL7 = recode(as.character(ADOPT_ENFAIL7), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL7)

## SEXE_ENFAIL8
freq(BIEF$SEXE_ENFAIL8)
BIEF <- BIEF %>% mutate(SEXE_ENFAIL8 = recode(as.character(SEXE_ENFAIL8), "1" = "Masculin", "2" = "Féminin"))
freq(BIEF$SEXE_ENFAIL8)

## NEFRANCE_ENFAIL8
freq(BIEF$NEFRANCE_ENFAIL8)
BIEF <- BIEF %>% mutate(NEFRANCE_ENFAIL8 = recode(as.character(NEFRANCE_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$NEFRANCE_ENFAIL8)

## PARENT_VIT_ENFAIL8
freq(BIEF$PARENT_VIT_ENFAIL8)
BIEF <- BIEF %>% mutate(PARENT_VIT_ENFAIL8 = recode(as.character(PARENT_VIT_ENFAIL8), "1" = "Oui, il/elle vit avec vous", "2" = "Non, il/elle vit ailleurs", "3" = "Non, il/elle est décédé(e)", "4" = "Je ne sais pas Questionnaire BVA : « Vous ne savez pas »", "5" = "Il n'y a pas d'autre parent"))
freq(BIEF$PARENT_VIT_ENFAIL8)

## PARENT_VECU_ENFAIL8
freq(BIEF$PARENT_VECU_ENFAIL8)
BIEF <- BIEF %>% mutate(PARENT_VECU_ENFAIL8 = recode(as.character(PARENT_VECU_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$PARENT_VECU_ENFAIL8)

## DC_ENFAIL8
freq(BIEF$DC_ENFAIL8)
BIEF <- BIEF %>% mutate(DC_ENFAIL8 = recode(as.character(DC_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$DC_ENFAIL8)

## PARENT_FREQ_ENFAIL8
freq(BIEF$PARENT_FREQ_ENFAIL8)
BIEF <- BIEF %>% mutate(PARENT_FREQ_ENFAIL8 = recode(as.character(PARENT_FREQ_ENFAIL8), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$PARENT_FREQ_ENFAIL8)

## FR_ENFAIL8
freq(BIEF$FR_ENFAIL8)
BIEF <- BIEF %>% mutate(FR_ENFAIL8 = recode(as.character(FR_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_ENFAIL8)

## LOG_ENFAIL8
freq(BIEF$LOG_ENFAIL8)
BIEF <- BIEF %>% mutate(LOG_ENFAIL8 = recode(as.character(LOG_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$LOG_ENFAIL8)

## AUT_PAR_ENFAIL8
freq(BIEF$AUT_PAR_ENFAIL8)
BIEF <- BIEF %>% mutate(AUT_PAR_ENFAIL8 = recode(as.character(AUT_PAR_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$AUT_PAR_ENFAIL8)

## DORT_ENFAIL8
freq(BIEF$DORT_ENFAIL8)
BIEF <- BIEF %>% mutate(DORT_ENFAIL8 = recode(as.character(DORT_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$DORT_ENFAIL8)

## SEP_AUT_PAR_ENFAIL8
freq(BIEF$SEP_AUT_PAR_ENFAIL8)
BIEF <- BIEF %>% mutate(SEP_AUT_PAR_ENFAIL8 = recode(as.character(SEP_AUT_PAR_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$SEP_AUT_PAR_ENFAIL8)

## RESID_ENFAIL8
freq(BIEF$RESID_ENFAIL8)
BIEF <- BIEF %>% mutate(RESID_ENFAIL8 = recode(as.character(RESID_ENFAIL8), "1" = "Pas de décision de justice", "2" = "La moitié du temps chez chaque parentQuestionnaire BVA : « La moitié du temps chez chaque parent (garde alternée) »", "3" = "Principalement chez vous", "4" = "Principalement chez son autre parent"))
freq(BIEF$RESID_ENFAIL8)

## SANTE_ENFAIL8
freq(BIEF$SANTE_ENFAIL8)
BIEF <- BIEF %>% mutate(SANTE_ENFAIL8 = recode(as.character(SANTE_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$SANTE_ENFAIL8)

## ASE_ENFAIL8
freq(BIEF$ASE_ENFAIL8)
BIEF <- BIEF %>% mutate(ASE_ENFAIL8 = recode(as.character(ASE_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$ASE_ENFAIL8)

## ADOPT_ENFAIL8
freq(BIEF$ADOPT_ENFAIL8)
BIEF <- BIEF %>% mutate(ADOPT_ENFAIL8 = recode(as.character(ADOPT_ENFAIL8), "1" = "Oui", "2" = "Non"))
freq(BIEF$ADOPT_ENFAIL8)

## SEXE_PAR1
freq(BIEF$SEXE_PAR1)
BIEF <- BIEF %>% mutate(SEXE_PAR1 = recode(as.character(SEXE_PAR1), "1" = "Une femme", "2" = "Un homme"))
freq(BIEF$SEXE_PAR1)

## SEXPAR1
freq(BIEF$SEXPAR1)
BIEF <- BIEF %>% mutate(SEXPAR1 = recode(as.character(SEXPAR1), "1" = "Une femme", "2" = "Un homme"))
freq(BIEF$SEXPAR1)

## NATIO_PAR1
freq(BIEF$NATIO_PAR1)
BIEF <- BIEF %>% mutate(NATIO_PAR1 = recode(as.character(NATIO_PAR1), "1" = "Oui", "2" = "Non", "3" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "4" = "Je ne souhaite pas répondreQuestionnaire BVA : « Vous ne souhaitez pas répondre »"))
freq(BIEF$NATIO_PAR1)

## TRAV_PAR1
freq(BIEF$TRAV_PAR1)
BIEF <- BIEF %>% mutate(TRAV_PAR1 = recode(as.character(TRAV_PAR1), "1" = "Oui", "2" = "Non"))
freq(BIEF$TRAV_PAR1)

## STATUT_PAR1
freq(BIEF$STATUT_PAR1)
BIEF <- BIEF %>% mutate(STATUT_PAR1 = recode(as.character(STATUT_PAR1), "1" = "A son compte (y compris gérant(e) de société salarié(e)", "2" = "Salarié(e) de la fonction publique (Etat, territoriale, hospitalière)", "3" = "Salarié(e) d'une entreprise (y compris d'une association ou de la sécurité sociale)", "4" = "Salarié(e) d'un particulier", "5" = "Aide familial(e) non rémunéré(e)", "6" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »"))
freq(BIEF$STATUT_PAR1)

## LANGUE2_PAR1E
freq(BIEF$LANGUE2_PAR1E)
BIEF <- BIEF %>% mutate(LANGUE2_PAR1E = recode(as.character(LANGUE2_PAR1E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE2_PAR1E)

## LANGUE3_PAR1E
freq(BIEF$LANGUE3_PAR1E)
BIEF <- BIEF %>% mutate(LANGUE3_PAR1E = recode(as.character(LANGUE3_PAR1E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE3_PAR1E)

## LANGUE4_PAR1E
freq(BIEF$LANGUE4_PAR1E)
BIEF <- BIEF %>% mutate(LANGUE4_PAR1E = recode(as.character(LANGUE4_PAR1E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE4_PAR1E)

## VIV_PAR1
freq(BIEF$VIV_PAR1)
BIEF <- BIEF %>% mutate(VIV_PAR1 = recode(as.character(VIV_PAR1), "1" = "Oui", "2" = "Non"))
freq(BIEF$VIV_PAR1)

## LOG_PAR1
freq(BIEF$LOG_PAR1)
BIEF <- BIEF %>% mutate(LOG_PAR1 = recode(as.character(LOG_PAR1), "1" = "Avec vous, dans ce logement", "2" = "Ailleurs"))
freq(BIEF$LOG_PAR1)

## FR_PAR1
freq(BIEF$FR_PAR1)
BIEF <- BIEF %>% mutate(FR_PAR1 = recode(as.character(FR_PAR1), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_PAR1)

## ETAB_PAR1
freq(BIEF$ETAB_PAR1)
BIEF <- BIEF %>% mutate(ETAB_PAR1 = recode(as.character(ETAB_PAR1), "1" = "Oui", "2" = "Non"))
freq(BIEF$ETAB_PAR1)

## FREQ_VU_PAR1
freq(BIEF$FREQ_VU_PAR1)
BIEF <- BIEF %>% mutate(FREQ_VU_PAR1 = recode(as.character(FREQ_VU_PAR1), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$FREQ_VU_PAR1)

## EXIST_PAR2
freq(BIEF$EXIST_PAR2)
BIEF <- BIEF %>% mutate(EXIST_PAR2 = recode(as.character(EXIST_PAR2), "1" = "Oui", "2" = "Non"))
freq(BIEF$EXIST_PAR2)

## SEXE_PAR2
freq(BIEF$SEXE_PAR2)
BIEF <- BIEF %>% mutate(SEXE_PAR2 = recode(as.character(SEXE_PAR2), "1" = "Une femme", "2" = "Un homme"))
freq(BIEF$SEXE_PAR2)

## SEXPAR2
freq(BIEF$SEXPAR2)
BIEF <- BIEF %>% mutate(SEXPAR2 = recode(as.character(SEXPAR2), "1" = "Une femme", "2" = "Un homme"))
freq(BIEF$SEXPAR2)

## NATIO_PAR2
freq(BIEF$NATIO_PAR2)
BIEF <- BIEF %>% mutate(NATIO_PAR2 = recode(as.character(NATIO_PAR2), "1" = "Oui", "2" = "Non", "3" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »", "4" = "Je ne souhaite pas répondreQuestionnaire BVA : « Vous ne souhaitez pas répondre »"))
freq(BIEF$NATIO_PAR2)

## TRAV_PAR2
freq(BIEF$TRAV_PAR2)
BIEF <- BIEF %>% mutate(TRAV_PAR2 = recode(as.character(TRAV_PAR2), "1" = "Oui", "2" = "Non"))
freq(BIEF$TRAV_PAR2)

## STATUT_PAR2
freq(BIEF$STATUT_PAR2)
BIEF <- BIEF %>% mutate(STATUT_PAR2 = recode(as.character(STATUT_PAR2), "1" = "A son compte (y compris gérant(e) de société salarié(e)", "2" = "Salarié(e) de la fonction publique (Etat, territoriale, hospitalière)", "3" = "Salarié(e) d'une entreprise (y compris d'une association ou de la sécurité sociale)", "4" = "Salarié(e) d'un particulier", "5" = "Aide familial(e) non rémunéré(e)", "6" = "Je ne sais pasQuestionnaire BVA : « Vous ne savez pas »"))
freq(BIEF$STATUT_PAR2)

## LANGUE2_PAR2E
freq(BIEF$LANGUE2_PAR2E)
BIEF <- BIEF %>% mutate(LANGUE2_PAR2E = recode(as.character(LANGUE2_PAR2E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE2_PAR2E)

## LANGUE3_PAR2E
freq(BIEF$LANGUE3_PAR2E)
BIEF <- BIEF %>% mutate(LANGUE3_PAR2E = recode(as.character(LANGUE3_PAR2E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE3_PAR2E)

## LANGUE4_PAR2E
freq(BIEF$LANGUE4_PAR2E)
BIEF <- BIEF %>% mutate(LANGUE4_PAR2E = recode(as.character(LANGUE4_PAR2E), "1" = "Oui", "2" = "Non"))
freq(BIEF$LANGUE4_PAR2E)

## VIV_PAR2
freq(BIEF$VIV_PAR2)
BIEF <- BIEF %>% mutate(VIV_PAR2 = recode(as.character(VIV_PAR2), "1" = "Oui", "2" = "Non"))
freq(BIEF$VIV_PAR2)

## LOG_PAR2B
freq(BIEF$LOG_PAR2B)
BIEF <- BIEF %>% mutate(LOG_PAR2B = recode(as.character(LOG_PAR2B), "1" = "Avec vous, dans ce logement", "2" = "Ailleurs"))
freq(BIEF$LOG_PAR2B)

## FR_PAR2
freq(BIEF$FR_PAR2)
BIEF <- BIEF %>% mutate(FR_PAR2 = recode(as.character(FR_PAR2), "1" = "Oui", "2" = "Non"))
freq(BIEF$FR_PAR2)

## ETAB_PAR2
freq(BIEF$ETAB_PAR2)
BIEF <- BIEF %>% mutate(ETAB_PAR2 = recode(as.character(ETAB_PAR2), "1" = "Oui", "2" = "Non"))
freq(BIEF$ETAB_PAR2)

## FREQ_VU_PAR2
freq(BIEF$FREQ_VU_PAR2)
BIEF <- BIEF %>% mutate(FREQ_VU_PAR2 = recode(as.character(FREQ_VU_PAR2), "1" = "Une ou plusieurs fois par semaine", "2" = "Une ou plusieurs fois par mois", "3" = "Une ou plusieurs fois par an", "4" = "Plus rarement ou jamais"))
freq(BIEF$FREQ_VU_PAR2)

## NB_FRERES_VIV1
freq(BIEF$NB_FRERES_VIV1)
BIEF <- BIEF %>% mutate(NB_FRERES_VIV1 = recode(as.character(NB_FRERES_VIV1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NB_FRERES_VIV1)

## NB_SOEURS_VIV1
freq(BIEF$NB_SOEURS_VIV1)
BIEF <- BIEF %>% mutate(NB_SOEURS_VIV1 = recode(as.character(NB_SOEURS_VIV1), "1" = "Oui", "2" = "Non"))
freq(BIEF$NB_SOEURS_VIV1)

## PETIT_ENF
freq(BIEF$PETIT_ENF)
BIEF <- BIEF %>% mutate(PETIT_ENF = recode(as.character(PETIT_ENF), "1" = "Oui", "2" = "Non"))
freq(BIEF$PETIT_ENF)

