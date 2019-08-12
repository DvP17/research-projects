###########################################################################|
##################   D A T A   C O N S T R U C T I O N   ##################|
###########################################################################|

############################################################################
## Construct Matrices ######################################################

# Exiobase
setwd("C:/Data/Exiobase")
mrio::exioloop(1995:2015, "bl", "pd") -> exio_m_bl
mrio::exioloop(1995:2015, "bw", "pd") -> exio_m_bw
mrio::exioloop(1995:2015, "cc", "pd") -> exio_m_cc
mrio::exioloop(1995:2015, "en", "pd") -> exio_m_en
mrio::exioloop(1995:2015, "lu", "pd") -> exio_m_lu
mrio::exioloop(1995:2015, "mf", "pd") -> exio_m_mf
mrio::exioloop(1995:2015, "ws", "pd") -> exio_m_ws

save(exio_m_bl, file = "o_exio_m_bl.RData")
save(exio_m_bw, file = "o_exio_m_bw.RData")
save(exio_m_cc, file = "o_exio_m_cc.RData")
save(exio_m_en, file = "o_exio_m_en.RData")
save(exio_m_lu, file = "o_exio_m_lu.RData")
save(exio_m_mf, file = "o_exio_m_mf.RData")
save(exio_m_ws, file = "o_exio_m_ws.RData")

  # Exiobase PXP
  # mrio::exioloop(1995:2011, "bl", "pd", type = "pxp") -> exio_m_bl_pxp
  mrio::exioloop(1995:2011, "bw", "pd", type = "pxp") -> exio_m_bw_pxp
  mrio::exioloop(1995:2011, "cc", "pd", type = "pxp") -> exio_m_cc_pxp
  mrio::exioloop(1995:2011, "en", "pd", type = "pxp") -> exio_m_en_pxp
  mrio::exioloop(1995:2011, "lu", "pd", type = "pxp") -> exio_m_lu_pxp
  mrio::exioloop(1995:2011, "mf", "pd", type = "pxp") -> exio_m_mf_pxp
  mrio::exioloop(1995:2011, "ws", "pd", type = "pxp") -> exio_m_ws_pxp
  
  # save(exio_m_bl_pxp, file = "o_exio_m_bl_pxp.RData")
  save(exio_m_bw_pxp, file = "o_exio_m_bw_pxp.RData")
  save(exio_m_cc_pxp, file = "o_exio_m_cc_pxp.RData")
  save(exio_m_en_pxp, file = "o_exio_m_en_pxp.RData")
  save(exio_m_lu_pxp, file = "o_exio_m_lu_pxp.RData")
  save(exio_m_mf_pxp, file = "o_exio_m_mf_pxp.RData")
  save(exio_m_ws_pxp, file = "o_exio_m_ws_pxp.RData")


# Eora
setwd("C:/Data/Eora")
#mrio::eoraloop(1990:2015, "bl", "pd") -> eora_m_bl
mrio::eoraloop(1990:2015, "bw", "pd") -> eora_m_bw
mrio::eoraloop(1990:2015, "cc", "pd") -> eora_m_cc
mrio::eoraloop(1990:2015, "en", "pd") -> eora_m_en
mrio::eoraloop(1990:2015, "lu", "pd") -> eora_m_lu
mrio::eoraloop(1990:2015, "mf", "pd") -> eora_m_mf
#mrio::eoraloop(1990:2015, "ws", "pd") -> eora_m_ws

#save(eora_m_bl, file = "o_eora_m_bl.RData")
save(eora_m_bw, file = "o_eora_m_bw.RData")
save(eora_m_cc, file = "o_eora_m_cc.RData")
save(eora_m_en, file = "o_eora_m_en.RData")
save(eora_m_lu, file = "o_eora_m_lu.RData")
save(eora_m_mf, file = "o_eora_m_mf.RData")
#save(eora_m_ws, file = "o_eora_m_ws.RData")



############################################################################
## Construct Dyads #########################################################

# Exiobase
mrio::dyads(1995:2015, exio_m_bl) -> exio_d_bl
mrio::dyads(1995:2015, exio_m_bw) -> exio_d_bw
mrio::dyads(1995:2015, exio_m_cc) -> exio_d_cc
mrio::dyads(1995:2015, exio_m_en) -> exio_d_en
mrio::dyads(1995:2015, exio_m_lu) -> exio_d_lu
mrio::dyads(1995:2015, exio_m_mf) -> exio_d_mf
mrio::dyads(1995:2015, exio_m_ws) -> exio_d_ws

save(exio_d_bl, file = "o_exio_d_bl.RData")
save(exio_d_bw, file = "o_exio_d_bw.RData")
save(exio_d_cc, file = "o_exio_d_cc.RData")
save(exio_d_en, file = "o_exio_d_en.RData")
save(exio_d_lu, file = "o_exio_d_lu.RData")
save(exio_d_mf, file = "o_exio_d_mf.RData")
save(exio_d_ws, file = "o_exio_d_ws.RData")

  # Exiobase pxp
  # mrio::dyads(1995:2011, exio_m_bl_pxp) -> exio_d_bl_pxp
  mrio::dyads(1995:2011, exio_m_bw_pxp) -> exio_d_bw_pxp
  mrio::dyads(1995:2011, exio_m_cc_pxp) -> exio_d_cc_pxp
  mrio::dyads(1995:2011, exio_m_en_pxp) -> exio_d_en_pxp
  mrio::dyads(1995:2011, exio_m_lu_pxp) -> exio_d_lu_pxp
  mrio::dyads(1995:2011, exio_m_mf_pxp) -> exio_d_mf_pxp
  # mrio::dyads(1995:2011, exio_m_ws_pxp) -> exio_d_ws_pxp
  
  # save(exio_d_bl_pxp, file = "o_exio_d_bl_pxp.RData")
  save(exio_d_bw_pxp, file = "o_exio_d_bw_pxp.RData")
  save(exio_d_cc_pxp, file = "o_exio_d_cc_pxp.RData")
  save(exio_d_en_pxp, file = "o_exio_d_en_pxp.RData")
  save(exio_d_lu_pxp, file = "o_exio_d_lu_pxp.RData")
  save(exio_d_mf_pxp, file = "o_exio_d_mf_pxp.RData")
  # save(exio_d_ws_pxp, file = "o_exio_d_ws_pxp.RData")


# Eora
#mrio::dyads(1990:2015, eora_m_bl) -> eora_d_bl
mrio::dyads(1990:2015, eora_m_bw) -> eora_d_bw
mrio::dyads(1990:2015, eora_m_cc) -> eora_d_cc
mrio::dyads(1990:2015, eora_m_en) -> eora_d_en
mrio::dyads(1990:2015, eora_m_lu) -> eora_d_lu
mrio::dyads(1990:2015, eora_m_mf) -> eora_d_mf
#mrio::dyads(1990:2015, eora_m_ws) -> eora_d_ws

#save(eora_d_bl, file = "o_eora_d_bl.RData")
save(eora_d_bw, file = "o_eora_d_bw.RData")
save(eora_d_cc, file = "o_eora_d_cc.RData")
save(eora_d_en, file = "o_eora_d_en.RData")
save(eora_d_lu, file = "o_eora_d_lu.RData")
save(eora_d_mf, file = "o_eora_d_mf.RData")
#save(eora_d_ws, file = "o_eora_d_ws.RData")

