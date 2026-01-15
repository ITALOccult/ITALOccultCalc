using System;
using System.ComponentModel;
using System.Drawing.Imaging;
using System.IO;
using System.Net;
using System.Net.Mail;
using System.Net.Mime;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	internal class Emails
	{
		internal static string MessageText = "";

		internal static string GrazeMessageText = "";

		internal static DownloadAddresses DownLoadAddress;

		internal static bool IncludeILOCRevisionMessage = false;

		private static string ILOCmessage = Utilities.AppPath + "\\Resource Files\\Lunar Reduction Email body ILOC.txt";

		internal static string Email_Reduction_Report(string SpecialMessage, bool IncludeProfile, bool IncludeObservers)
		{
			//IL_00dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Invalid comparison between Unknown and I4
			string text = "Dear " + LunarObservations.OccMain.Representative + "\r\n";
			string subject = "Lunar occultation reductions";
			string to = LunarObservations.OccMain.EMail.Trim();
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
			string eMailServerName = Settings.Default.EMailServerName;
			string FileName;
			string text2 = LunarObservations.List_Residuals.FileOfObservations(out FileName);
			if (text2.Length != 0)
			{
				subject = "Reductions of " + FileName;
				text2 += "-";
			}
			string text3 = Utilities.AppPath + "\\Lunar Archive\\" + text2 + "Reduction Report.txt";
			string text4 = Utilities.AppPath + "\\Lunar Archive\\Reduction Profile.png";
			string text5 = "";
			if (IncludeILOCRevisionMessage && File.Exists(ILOCmessage) && (int)MessageBox.Show("Include the special ILOC revision message?", "ILOC revision", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				using StreamReader streamReader = new StreamReader(ILOCmessage);
				text += streamReader.ReadToEnd();
			}
			if (SpecialMessage.Length > 0)
			{
				text = text + "\r\n**** Special message ****\r\n" + SpecialMessage + "\r\n*******\r\n\r\n";
			}
			using (StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\Lunar Reduction Email body.txt"))
			{
				text += streamReader2.ReadToEnd();
			}
			MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, to, subject, text);
			mailMessage.Bcc.Add(fTP_AnonymousPassword);
			if (IncludeObservers)
			{
				for (int i = 0; i < LunarObservations.OccMain.Observers.Count; i++)
				{
					string text6 = LunarObservations.OccMain.Observers[i].ObserverEmail.ToString();
					if ((text6.Length > 5) & text6.Contains("@"))
					{
						mailMessage.CC.Add(text6);
					}
				}
			}
			using (StreamWriter streamWriter = new StreamWriter(text3, append: false))
			{
				streamWriter.Write(LunarObservations.List_Residuals.CollectEvents(FullWidth: false));
			}
			Attachment attachment = new Attachment(text3);
			mailMessage.Attachments.Add(attachment);
			if (IncludeProfile && ((Control)ReductionProfile.ObservedProfile).get_Created())
			{
				ReductionProfile.ObservedProfile.picProfile.get_Image().Save(text4, ImageFormat.Png);
				Attachment item = new Attachment(text4);
				mailMessage.Attachments.Add(item);
			}
			SmtpClient smtpClient = new SmtpClient(eMailServerName);
			if (Settings.Default.Email.Length > 0)
			{
				smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
			}
			smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
			if (!int.TryParse(Settings.Default.EmailPort, out var result))
			{
				result = -1;
			}
			if (result >= 0)
			{
				smtpClient.Port = result;
			}
			try
			{
				smtpClient.Send(mailMessage);
			}
			catch (SmtpFailedRecipientsException ex)
			{
				for (int j = 0; j < ex.InnerExceptions.Length; j++)
				{
					text5 = text5 + "Failed to deliver message to " + ex.FailedRecipient![j] + "\r\n";
				}
			}
			catch (SmtpException ex2)
			{
				text5 = text5 + ex2.Message + "\r\n\r\n";
			}
			attachment.Dispose();
			mailMessage.Dispose();
			if (File.Exists(text3))
			{
				File.Delete(text3);
			}
			if (File.Exists(text4))
			{
				File.Delete(text4);
			}
			return text5;
		}

		internal static string Email_Observation_Report()
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			string text = "";
			EmailObservations emailObservations = new EmailObservations();
			if ((int)((Form)emailObservations).ShowDialog() == 1)
			{
				string text2 = LunarObservations.OccMain.Events[0].ToString().Substring(0, 8).Replace(' ', '0');
				if (EventLine.UseOldFormat)
				{
					text2 = LunarObservations.OccMain.Events[0].ToString().Substring(2, 8).Replace(' ', '0');
				}
				string body = "Occultation report attached\r\n\r\n" + LunarObservations.OccMain.Representative;
				string subject = ((!emailObservations.IsGraze) ? ("Lunar occn obsns, " + text2 + "+, " + LunarObservations.OccMain.Representative.Replace(" ", "").Replace(".", "_")) : ("Graze obsn, " + LunarObservations.OccMain.Events[0].ToString().Substring(0, 8).Replace(' ', '0') + ", " + LunarObservations.OccMain.Representative.Replace(" ", "").Replace(".", "_")));
				string text3 = ((Control)emailObservations.txtAddress).get_Text();
				string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
				string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
				string eMailServerName = Settings.Default.EMailServerName;
				string text4 = string.Concat(str2: text2 + LunarObservations.OccMain.Representative.Replace(" ", "").Replace(".", "_") + ".txt", str0: Utilities.AppPath, str1: "\\Lunar Archive\\");
				MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, text3, subject, body);
				mailMessage.Bcc.Add(fTP_AnonymousPassword);
				LunarObservations.OccMain.WriteReport(text4, UseOldFormat: false);
				Attachment attachment = new Attachment(text4);
				mailMessage.Attachments.Add(attachment);
				SmtpClient smtpClient = new SmtpClient(eMailServerName);
				if (Settings.Default.Email.Length > 0)
				{
					smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
				}
				smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
				if (!int.TryParse(Settings.Default.EmailPort, out var result))
				{
					result = -1;
				}
				if (result >= 0)
				{
					smtpClient.Port = result;
				}
				try
				{
					smtpClient.Send(mailMessage);
				}
				catch (SmtpFailedRecipientsException ex)
				{
					for (int i = 0; i < ex.InnerExceptions.Length; i++)
					{
						text = text + "Failed to deliver message to " + ex.FailedRecipient![i] + "\r\n";
					}
				}
				catch (SmtpException ex2)
				{
					text = text + ex2.Message + "\r\n\r\n";
				}
				attachment.Dispose();
				mailMessage.Dispose();
				if (File.Exists(text4))
				{
					File.Delete(text4);
				}
			}
			else
			{
				text = "Aborted - no address selected";
			}
			((Component)(object)emailObservations).Dispose();
			return text;
		}

		internal static void GetCurrentAddresses()
		{
			//IL_000f: Unknown result type (might be due to invalid IL or missing references)
			DownLoadAddress = new DownloadAddresses();
			((Form)DownLoadAddress).ShowDialog();
		}

		internal static string Email_Graze_Query()
		{
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_0019: Invalid comparison between Unknown and I4
			string text = "";
			string subject = "Message to observers";
			GrazeMessage grazeMessage = new GrazeMessage();
			if ((int)((Form)grazeMessage).ShowDialog() == 1)
			{
				for (int i = 0; i < LunarObservations.OccMain.Events.Count; i++)
				{
					if (LunarObservations.OccMain.Events[i].GrazeFlag == "G")
					{
						subject = "Graze of " + LunarObservations.OccMain.Events[i].StarCat + LunarObservations.OccMain.Events[i].StarNumber + " on " + Utilities.Date_from_JD(LunarObservations.OccMain.Events[i].EventJD0Hrs);
						break;
					}
				}
				MailMessage mailMessage = new MailMessage();
				mailMessage.Body = ((Control)grazeMessage.txtMessage).get_Text() + "\r\n";
				mailMessage.Subject = subject;
				for (int j = 0; j < ((ObjectCollection)grazeMessage.lstObservers.get_Items()).get_Count(); j++)
				{
					if (grazeMessage.lstObservers.GetItemChecked(j))
					{
						mailMessage.To.Add(grazeMessage.Addresses[j]!.ToString());
					}
				}
				mailMessage.Bcc.Add(Settings.Default.FTP_AnonymousPassword);
				mailMessage.From = new MailAddress(Settings.Default.FTP_AnonymousPassword);
				string text2 = Utilities.AppPath + "\\Lunar Archive\\Observation Report.txt";
				string text3 = Utilities.AppPath + "\\Lunar Archive\\Reduction Report.txt";
				string text4 = Utilities.AppPath + "\\Lunar Archive\\Reduction Profile.png";
				if (grazeMessage.chkReport.get_Checked())
				{
					LunarObservations.OccMain.WriteReport(text2, UseOldFormat: false);
					Attachment item = new Attachment(text2);
					mailMessage.Attachments.Add(item);
				}
				if (grazeMessage.chkReductions.get_Checked())
				{
					using (StreamWriter streamWriter = new StreamWriter(text3, append: false))
					{
						streamWriter.Write(LunarObservations.List_Residuals.CollectEvents(FullWidth: false));
					}
					Attachment item2 = new Attachment(text3);
					mailMessage.Attachments.Add(item2);
				}
				if (grazeMessage.chkProfile.get_Checked() && ReductionProfile.ObservedProfile != null)
				{
					ReductionProfile.ObservedProfile.picProfile.get_Image().Save(text4, ImageFormat.Png);
					Attachment item3 = new Attachment(text4);
					mailMessage.Attachments.Add(item3);
				}
				SmtpClient smtpClient = new SmtpClient(Settings.Default.EMailServerName);
				if (Settings.Default.Email.Length > 0)
				{
					smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
				}
				smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
				if (!int.TryParse(Settings.Default.EmailPort, out var result))
				{
					result = -1;
				}
				if (result >= 0)
				{
					smtpClient.Port = result;
				}
				try
				{
					smtpClient.Send(mailMessage);
				}
				catch (SmtpFailedRecipientsException ex)
				{
					for (int k = 0; k < ex.InnerExceptions.Length; k++)
					{
						text = text + "Failed to deliver message to " + ex.FailedRecipient![k] + "\r\n";
					}
				}
				catch (SmtpException ex2)
				{
					text = text + ex2.Message + "\r\n\r\n";
				}
				mailMessage.Dispose();
				if (File.Exists(text2))
				{
					File.Delete(text2);
				}
				if (File.Exists(text3))
				{
					File.Delete(text3);
				}
				if (File.Exists(text4))
				{
					File.Delete(text4);
				}
			}
			else
			{
				text = "Email cancelled";
			}
			((Component)(object)grazeMessage).Dispose();
			return text;
		}

		internal static string Email_DoubleStarReport(string Observations, bool IncludeImage, string Subject)
		{
			string text = "";
			string[] array = ((Control)LunarObservations.DoubleStarReport.txt_cc_Addresses).get_Text().Split(new char[1] { ';' });
			string text2 = ((Control)LunarObservations.DoubleStarReport.txtEmailAddress).get_Text();
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			string eMailServerName = Settings.Default.EMailServerName;
			MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword, text2, Subject, Observations);
			string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
			mailMessage.Bcc.Add(fTP_AnonymousPassword2);
			for (int i = 0; i < array.GetUpperBound(0); i++)
			{
				if (array[i].Trim().Contains("@"))
				{
					mailMessage.CC.Add(array[i].Trim());
				}
			}
			if (IncludeImage)
			{
				string text3 = Utilities.AppPath + "\\Observations\\LiMovieCurve.png";
				LunarObservations.DoubleStarReport.picLiMovie.get_Image().Save(text3, ImageFormat.Png);
				Attachment item = new Attachment(text3);
				mailMessage.Attachments.Add(item);
			}
			SmtpClient smtpClient = new SmtpClient(eMailServerName);
			if (Settings.Default.Email.Length > 0)
			{
				smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
			}
			smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
			if (!int.TryParse(Settings.Default.EmailPort, out var result))
			{
				result = -1;
			}
			if (result >= 0)
			{
				smtpClient.Port = result;
			}
			try
			{
				smtpClient.Send(mailMessage);
			}
			catch (SmtpFailedRecipientsException ex)
			{
				for (int j = 0; j < ex.InnerExceptions.Length; j++)
				{
					text = text + "Failed to deliver message to " + ex.FailedRecipient![j] + "\r\n";
				}
			}
			catch (SmtpException ex2)
			{
				text = text + ex2.Message + "\r\n\r\n";
			}
			mailMessage.Dispose();
			return text;
		}

		internal static string Email_LogFile(string LogFile, string Signature, string Subject)
		{
			string text = "";
			string path = Utilities.AppPath + "\\Resource Files\\addresses.txt";
			string text2 = "";
			if (File.Exists(path))
			{
				using (StreamReader streamReader = new StreamReader(path))
				{
					while (!streamReader.EndOfStream)
					{
						string text3 = streamReader.ReadLine();
						if (text3.ToUpper().Substring(0, 4) != "<SOU")
						{
							int num = text3.IndexOf(">");
							if ((num > 1) & (num < text3.Length - 1))
							{
								text2 = text2 + text3.Substring(num + 1) + ";";
							}
						}
					}
				}
				string body = "The file of recent lunar occultation observations has just been updated. The attached file lists the files used to create this latest consolidation.\r\n\r\n" + Signature + "\r\n" + DateTime.Now.ToLongDateString();
				string[] array = text2.Split(new char[1] { ';' });
				string to = array[0];
				string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
				string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
				string eMailServerName = Settings.Default.EMailServerName;
				MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, to, Subject, body);
				mailMessage.Bcc.Add(fTP_AnonymousPassword);
				for (int i = 1; i < array.GetUpperBound(0); i++)
				{
					mailMessage.To.Add(array[i].Trim());
				}
				Attachment attachment = new Attachment(LogFile);
				mailMessage.Attachments.Add(attachment);
				SmtpClient smtpClient = new SmtpClient(eMailServerName);
				if (Settings.Default.Email.Length > 0)
				{
					smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
				}
				smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
				if (!int.TryParse(Settings.Default.EmailPort, out var result))
				{
					result = -1;
				}
				if (result >= 0)
				{
					smtpClient.Port = result;
				}
				try
				{
					smtpClient.Send(mailMessage);
				}
				catch (SmtpFailedRecipientsException ex)
				{
					for (int j = 0; j < ex.InnerExceptions.Length; j++)
					{
						text = text + "Failed to deliver message to " + ex.FailedRecipient![j] + "\r\n";
					}
				}
				catch (SmtpException ex2)
				{
					text = text + ex2.Message + "\r\n\r\n";
				}
				attachment.Dispose();
				mailMessage.Dispose();
				if (text.Trim().Length < 2)
				{
					text = "Email successfully sent.";
				}
				return text;
			}
			return "No address file";
		}

		internal static string Email_AsteroidAstrometry(string Message, string Subject, string Address)
		{
			string text = "";
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
			string eMailServerName = Settings.Default.EMailServerName;
			MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, Address)
			{
				Subject = Subject,
				Bcc = { fTP_AnonymousPassword },
				IsBodyHtml = false
			};
			using (AlternateView alternateView = AlternateView.CreateAlternateViewFromString(Message))
			{
				alternateView.TransferEncoding = TransferEncoding.SevenBit;
				mailMessage.AlternateViews.Add(alternateView);
				SmtpClient smtpClient = new SmtpClient(eMailServerName);
				if (Settings.Default.Email.Length > 0)
				{
					smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
				}
				smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
				if (!int.TryParse(Settings.Default.EmailPort, out var result))
				{
					result = -1;
				}
				if (result >= 0)
				{
					smtpClient.Port = result;
				}
				try
				{
					smtpClient.Send(mailMessage);
				}
				catch (SmtpFailedRecipientsException ex)
				{
					for (int i = 0; i < ex.InnerExceptions.Length; i++)
					{
						text = text + "Failed to deliver message to " + ex.FailedRecipient![i] + "\r\n";
					}
				}
				catch (SmtpException ex2)
				{
					text = text + ex2.Message + "\r\n\r\n";
				}
			}
			mailMessage.Dispose();
			return text;
		}
	}
}
