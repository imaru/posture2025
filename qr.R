library(qrcode)
teams<-qrcode::qr_code('https://teams.microsoft.com/l/channel/19%3A4f82ee0d415949f992100296e5be174d%40thread.tacv2/2025%E7%A0%94%E7%A9%B6%E5%AE%A4%E9%85%8D%E5%B1%9E?groupId=33a5b6c6-c5bf-4661-8e09-bb7259ffa31c&tenantId=9b6ebc5d-2372-4099-a565-650bcbc3274f')
plot(teams)

qualt<-qr_code('https://kanazawait.qualtrics.com/jfe/form/SV_0ulFhfiNwuNtxcO')
plot(qualt)
