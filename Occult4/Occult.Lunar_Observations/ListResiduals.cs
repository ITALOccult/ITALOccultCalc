using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ListResiduals : Form
	{
		internal string SourceFile;

		internal string ArchivePath = Utilities.AppPath + "\\Lunar Archive";

		internal string Mean_SD_Text = "";

		internal string Mean_SDSecs = "";

		internal bool CorrectionApplied;

		private ArrayList Archive;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withResidualsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem bySequenceNumberToolStripMenuItem;

		private ToolStripMenuItem byObserverToolStripMenuItem;

		private ToolStripMenuItem byTimeToolStripMenuItem;

		private ToolStripMenuItem byResidualToolStripMenuItem;

		private ToolStripMenuItem byPAToolStripMenuItem;

		private ToolStripMenuItem byStarNumberToolStripMenuItem;

		private ToolStripMenuItem byAxisAngleToolStripMenuItem;

		private ToolStripMenuItem byPAngleToolStripMenuItem;

		private ToolStripMenuItem byObserverInitialToolStripMenuItem;

		internal ListBox lstResiduals;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem displayEventAgainstProfileToolStripMenuItem;

		private Label LabelRightClick;

		private ToolStripMenuItem mnuAddToLocalArchive;

		private ToolStripMenuItem emailResultsAddToArchiveToolStripMenuItem;

		private ToolStripMenuItem emailResultsonlyToolStripMenuItem;

		private ToolStripMenuItem addToArchiveonlyToolStripMenuItem;

		private ToolStripMenuItem emailReductionsWithAMessageToolStripMenuItem;

		private ToolStripMenuItem updateEmailAddressesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem grazeCoordinatorFunctionsToolStripMenuItem;

		private ToolStripMenuItem sendEmailToOneOrMoreObserversToolStripMenuItem;

		private ToolStripMenuItem copyfullWidthToolStripMenuItem;

		internal Button cmdPlot;

		public ListResiduals()
		{
			InitializeComponent();
			((ToolStripItem)mnuAddToLocalArchive).set_Enabled(Settings.Default.AdministratorRegional);
		}

		private void ListResiduals_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		internal void DisplayResiduals()
		{
			bool flag = false;
			lstResiduals.get_Items().Clear();
			lstResiduals.get_Items().Add((object)("File name      : " + SourceFile));
			lstResiduals.get_Items().Add((object)("Reduction date : " + DateTime.Now.Date.ToString("D")));
			lstResiduals.get_Items().Add((object)("Ephemeris      : " + Utilities.EphemerisBasis()));
			if (Utilities.LOLAFileExists)
			{
				lstResiduals.get_Items().Add((object)"Limb basis     : LRO Lunar Orbiter Laser Altimeter [LOLA]");
			}
			else
			{
				lstResiduals.get_Items().Add((object)"Limb basis     : None");
			}
			lstResiduals.get_Items().Add((object)"O-C basis      : limb correction applied");
			lstResiduals.get_Items().Add((object)"");
			lstResiduals.get_Items().Add((object)"Telescopes:");
			lstResiduals.get_Items().Add((object)" Aperture   Longitude      Latitude   Alt");
			lstResiduals.get_Items().Add((object)"#   cm       o  '   \"      o  '   \"     m");
			for (int i = 0; i < LunarObservations.OccMain.Telescopes.Count; i++)
			{
				lstResiduals.get_Items().Add((object)LunarObservations.OccMain.Telescopes[i].ToShortString());
			}
			lstResiduals.get_Items().Add((object)"");
			lstResiduals.get_Items().Add((object)"ref Tel Observer             Star No.     y  m  d  h  m   s    PhGrMrCeDb     O-C      O-C   limb   PA      l     b     AA      P     D    scale  Src");
			lstResiduals.get_Items().Add((object)"                                                                              mas      sec    \"      o      o     o      o      o     o           cat.");
			Emails.IncludeILOCRevisionMessage = false;
			for (int j = 0; j < LunarObservations.Residuals.Count; j++)
			{
				lstResiduals.get_Items().Add((object)LunarObservations.Residuals[j].ToString());
				flag |= LunarObservations.Residuals[j].GrazeFlag != " ";
				if ((j + 1) % 5 == 0)
				{
					lstResiduals.get_Items().Add((object)"");
				}
				if ((LunarObservations.Residuals[j].Year > 2000) & (LunarObservations.Residuals[j].Year < 2008))
				{
					Emails.IncludeILOCRevisionMessage = true;
				}
			}
			lstResiduals.get_Items().Add((object)"");
			lstResiduals.get_Items().Add((object)"Star positions not from Gaia or Hipparcos2 have a '§' after the O-C");
			if (Mean_SD_Text.Length > 0)
			{
				lstResiduals.get_Items().Add((object)"");
				lstResiduals.get_Items().Add((object)Mean_SD_Text);
				if (Mean_SDSecs.Length > 0)
				{
					lstResiduals.get_Items().Add((object)Mean_SDSecs);
				}
				lstResiduals.get_Items().Add((object)"");
				lstResiduals.get_Items().Add((object)"Mean residuals are exclusive of:");
				lstResiduals.get_Items().Add((object)"    events after 1900 with a residual > ±0.5\"");
				lstResiduals.get_Items().Add((object)"    Bright limb events, events that are not 'Certain', and Start, End and Miss events");
				lstResiduals.get_Items().Add((object)"    events involving Planets or Asteroids");
				lstResiduals.get_Items().Add((object)"Mean clock correction additionally limited to events with a radial motion >0.15\"/sec");
			}
			lstResiduals.get_Items().Add((object)"");
			lstResiduals.get_Items().Add((object)"");
			lstResiduals.get_Items().Add((object)"Explanation of columns 'PhGrMrCeDb'");
			lstResiduals.get_Items().Add((object)"Ph - Phase of the event.");
			lstResiduals.get_Items().Add((object)"     1st character D = disappear, R = reappear, B = blink, F = flash.");
			lstResiduals.get_Items().Add((object)"          plus for grazes, M = Miss, S = Start, E = End");
			lstResiduals.get_Items().Add((object)"     2nd character D = dark limb, B = bright limb, U = in umbra of lunar eclipse");
			lstResiduals.get_Items().Add((object)"Gr - G if the event is during a graze");
			lstResiduals.get_Items().Add((object)"Mr - Method of timing and recording. Main types are:");
			lstResiduals.get_Items().Add((object)"     G = video with time insertion, V = video with other time linking");
			lstResiduals.get_Items().Add((object)"     S = visual using a stopwatch, T = visual using a tape recorder, E = eye/ear");
			lstResiduals.get_Items().Add((object)"Ce - Certainty. 1 = certain, 2 = may be spurious, 3 = most likely spurious");
			lstResiduals.get_Items().Add((object)"Db - Double star indication - West, East, North, South, Brighter, Fainter");
			((Control)cmdPlot).set_Enabled(flag);
		}

		private void bySequenceNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 0;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byObserverToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 1;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byObserverInitialToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 2;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byStarNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 3;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byTimeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 4;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byResidualToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 5;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byPAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 6;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byAxisAngleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 7;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void byPAngleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReductionLine.SortField = 8;
			LunarObservations.Residuals.Sort();
			DisplayResiduals();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(FullWidth: false));
		}

		private void copyfullWidthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(FullWidth: true));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(FullWidth: false));
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(FullWidth: false));
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarObservations = Output.SavePredictionText(CollectEvents(FullWidth: false), "Occultation Residuals", Settings.Default.Save_LunarObservations);
		}

		internal string CollectEvents(bool FullWidth)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstResiduals.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				string text = lstResiduals.get_Items().get_Item(i).ToString();
				if (text.Length > 92 && !FullWidth)
				{
					text = text.Substring(0, 92);
				}
				stringBuilder.AppendLine(text);
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		internal string FileOfObservations(out string FileName)
		{
			string text = "";
			FileName = "";
			if (lstResiduals.get_Items().get_Count() > 0)
			{
				text = lstResiduals.get_Items().get_Item(0).ToString();
				int num = text.LastIndexOf(":");
				if (num > 0)
				{
					FileName = text.Substring(num + 1).Trim();
					int num2 = text.LastIndexOf(".");
					if (num2 - num - 2 > 0 && num2 > 17)
					{
						text = text.Substring(num + 2, num2 - num - 2);
					}
				}
			}
			return text;
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			ReductionProfile.Show_ReductionProfile(-1);
		}

		private void ListResiduals_Resize(object sender, EventArgs e)
		{
			((Control)lstResiduals).set_Height(((Control)this).get_Height() - 103);
			((Control)lstResiduals).set_Width(((Control)this).get_Width() - 32);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Residuals");
		}

		private void lstResiduals_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Invalid comparison between Unknown and I4
			if (lstResiduals.get_Items().get_Count() >= 1 && (int)e.get_Button() == 2097152)
			{
				int num = lstResiduals.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstResiduals.get_Items().get_Count())
				{
					((ListControl)lstResiduals).set_SelectedIndex(num);
				}
				((Control)lstResiduals).Refresh();
			}
		}

		private void displayEventAgainstProfileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int num = -1;
			string text = lstResiduals.get_Items().get_Item(((ListControl)lstResiduals).get_SelectedIndex()).ToString();
			for (int i = 0; i < LunarObservations.Residuals.Count; i++)
			{
				if (LunarObservations.Residuals[i].ToString() == text)
				{
					num = i;
					break;
				}
			}
			if (num >= 0)
			{
				ReductionProfile.Show_ReductionProfile(num);
			}
		}

		private bool AddToArchive()
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007a: Invalid comparison between Unknown and I4
			if (!Directory.Exists(ArchivePath))
			{
				Directory.CreateDirectory(ArchivePath);
			}
			bool result = false;
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Add observations to an Archive file");
			((FileDialog)val).set_DefaultExt(".txt");
			((FileDialog)val).set_CheckFileExists(false);
			val.set_OverwritePrompt(false);
			((FileDialog)val).set_Filter("Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			((FileDialog)val).set_InitialDirectory(ArchivePath);
			((FileDialog)val).set_FileName(Settings.Default.LocalArchiveLastFile);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Archive = new ArrayList();
				Settings.Default.LocalArchiveLastFile = ((FileDialog)val).get_FileName();
				if (File.Exists(((FileDialog)val).get_FileName()))
				{
					string text = Path.GetDirectoryName(((FileDialog)val).get_FileName()) + "\\" + Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName()) + ".bup";
					string text2 = Path.GetDirectoryName(((FileDialog)val).get_FileName()) + "\\" + Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName()) + ".bp2";
					if (File.Exists(text))
					{
						if (File.Exists(text2))
						{
							File.Delete(text2);
						}
						File.Move(text, text2);
					}
					File.Copy(((FileDialog)val).get_FileName(), text);
					using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName());
					while (!streamReader.EndOfStream)
					{
						Archive.Add(streamReader.ReadLine());
					}
				}
				for (int i = 0; i < LunarObservations.OccMain.Events.Count; i++)
				{
					Archive.Add(LunarObservations.OccMain.ArchiveLine(i));
				}
				Archive.Sort();
				using (StreamWriter streamWriter = new StreamWriter(((FileDialog)val).get_FileName(), append: false))
				{
					for (int i = 0; i < Archive.Count; i++)
					{
						streamWriter.WriteLine(Archive[i]!.ToString());
					}
				}
				result = true;
			}
			return result;
		}

		private void emailResultsAddToArchiveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Invalid comparison between Unknown and I4
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0080: Invalid comparison between Unknown and I4
			//IL_00d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Invalid comparison between Unknown and I4
			//IL_014c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Unknown result type (might be due to invalid IL or missing references)
			//IL_0184: Unknown result type (might be due to invalid IL or missing references)
			//IL_018a: Invalid comparison between Unknown and I4
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a9: Invalid comparison between Unknown and I4
			//IL_01c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e8: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			bool flag = false;
			string observersWithEmails = GetObserversWithEmails();
			if (observersWithEmails.Length > 0 && (int)MessageBox.Show("Email will be sent to " + LunarObservations.OccMain.EMail + "\r\n\r\nDo you also want to copy the email to the following OBSERVERS? \r\n\r\n" + observersWithEmails, "Copy to observers", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				flag = true;
			}
			if (((Settings.Default.FTP_AnonymousPassword.Length < 5) | (Settings.Default.EMailServerName.Length < 5)) && (int)((Form)new SetEmailAndServer()).ShowDialog() == 2)
			{
				text = "Email message has been cancelled";
			}
			if (text.Length < 5)
			{
				string text2 = LunarObservations.OccMain.EMail;
				if (flag)
				{
					text2 = text2 + "  +\r\n" + observersWithEmails;
				}
				text = (((int)MessageBox.Show("The email will be sent to:\r\n\r\n" + text2 + "\r\n\r\nAre you sure you want to email the reductions?", "Confirm send", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7) ? "Email message has been cancelled" : ((!((LunarObservations.OccMain.EMail.Trim().Length > 0) & LunarObservations.OccMain.EMail.Contains("@"))) ? "Report has no email address, or an invalid email address - and has not been sent" : Emails.Email_Reduction_Report("", ((Control)cmdPlot).get_Enabled(), flag)));
			}
			if (text.Length < 5)
			{
				if (AddToArchive())
				{
					MessageBox.Show("Reduction has been emailed, and added to the archive", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				else
				{
					MessageBox.Show("Reduction has been emailed, but NOT added to the archive", "Successful email, not archived", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
			}
			else if ((int)MessageBox.Show(text + "\r\n\r\nDo you want to add the observations to the Archive now?", "Failed email", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				if ((int)MessageBox.Show("Are you sure you want to add the observations to the archive?", "Confirm add", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
				{
					if (AddToArchive())
					{
						MessageBox.Show("Reduction has been added to the archive, but has NOT been emailed.", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)64);
					}
					else
					{
						MessageBox.Show("Reduction has NOT been added to the archive, and has not been emailed", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)64);
					}
				}
			}
			else
			{
				MessageBox.Show("Reduction has NOT been added to the archive, and has not been emailed", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void emailResultsonlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Invalid comparison between Unknown and I4
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0080: Invalid comparison between Unknown and I4
			//IL_008f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010a: Invalid comparison between Unknown and I4
			//IL_013d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			bool flag = false;
			string observersWithEmails = GetObserversWithEmails();
			if (observersWithEmails.Length > 0 && (int)MessageBox.Show("Email will be sent to " + LunarObservations.OccMain.EMail + "\r\n\r\nDo you also want to copy the email to the following observers? \r\n\r\n" + observersWithEmails, "Copy to observers", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				flag = true;
			}
			if (((Settings.Default.FTP_AnonymousPassword.Length < 5) | (Settings.Default.EMailServerName.Length < 5)) && (int)((Form)new SetEmailAndServer()).ShowDialog() == 2)
			{
				MessageBox.Show("Message was cancelled. Email has not been sent.", "Cancelled email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((LunarObservations.OccMain.EMail.Trim().Length > 0) & LunarObservations.OccMain.EMail.Contains("@"))
			{
				string text2 = LunarObservations.OccMain.EMail;
				if (flag)
				{
					text2 = text2 + "  +\r\n" + observersWithEmails;
				}
				text = (((int)MessageBox.Show("The email will be sent to:\r\n\r\n" + text2 + "\r\n\r\nAre you sure you want to email the reductions?", "Confirm send", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7) ? Emails.Email_Reduction_Report("", ((Control)cmdPlot).get_Enabled(), flag) : "Email message has been cancelled");
				if (text.Length > 5)
				{
					MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				else
				{
					MessageBox.Show("Reduction has been sent", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
			}
			else
			{
				MessageBox.Show("Report has no email address, or an invalid email address", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
		}

		private void addToArchiveonlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0034: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_005c: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("Are you sure you want to add the observations to the archive?", "Confirm add", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				if (AddToArchive())
				{
					MessageBox.Show("Reduction has been added to the archive.", "Added to Archive", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				else
				{
					MessageBox.Show("Reduction has NOT been added to the archive.", "Cancelled archive addition", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
			}
			else
			{
				MessageBox.Show("Addition to archive has been cancelled.", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
		}

		private void emailReductionsWithAMessageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Invalid comparison between Unknown and I4
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0080: Invalid comparison between Unknown and I4
			//IL_008f: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Invalid comparison between Unknown and I4
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Unknown result type (might be due to invalid IL or missing references)
			//IL_014e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0169: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			string observersWithEmails = GetObserversWithEmails();
			if (observersWithEmails.Length > 0 && (int)MessageBox.Show("Email will be sent to " + LunarObservations.OccMain.EMail + "\r\n\r\nDo you also want to copy the email to the following observers? \r\n\r\n" + observersWithEmails, "Copy to observers", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				flag = true;
			}
			string text = "";
			if (((Settings.Default.FTP_AnonymousPassword.Length < 5) | (Settings.Default.EMailServerName.Length < 5)) && (int)((Form)new SetEmailAndServer()).ShowDialog() == 2)
			{
				MessageBox.Show("Message was cancelled. Email has not been sent.", "Cancelled email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((LunarObservations.OccMain.EMail.Trim().Length > 0) & LunarObservations.OccMain.EMail.Contains("@"))
			{
				string text2 = LunarObservations.OccMain.EMail;
				if (flag)
				{
					text2 = text2 + "  +\r\n" + observersWithEmails;
				}
				MessageText messageText = new MessageText(text2);
				if ((int)((Form)messageText).ShowDialog() == 1)
				{
					text = Emails.Email_Reduction_Report(((Control)messageText.txtMessage).get_Text(), ((Control)cmdPlot).get_Enabled(), flag);
					if (text.Length > 5)
					{
						MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
					}
					else
					{
						MessageBox.Show("Reduction has been sent", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
					}
				}
				else
				{
					MessageBox.Show("Message was cancelled. Email has not been sent.", "Cancelled email", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				((Component)(object)messageText).Dispose();
			}
			else
			{
				MessageBox.Show("Report has no email address, or an invalid email address", "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
		}

		private string GetObserversWithEmails()
		{
			string text = "";
			int num = -1;
			for (int i = 0; i < LunarObservations.OccMain.Observers.Count; i++)
			{
				if (LunarObservations.OccMain.Observers[i].ObserverEmail.ToString().Contains("@"))
				{
					num++;
					if (num % 5 == 0 && num > 0)
					{
						text += "\r\n";
					}
					text = text + LunarObservations.OccMain.Observers[i].ObserverName.Trim().ToString() + "     ";
				}
			}
			return text;
		}

		private void updateEmailAddressesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Emails.GetCurrentAddresses();
		}

		private void sendEmailToOneOrMoreObserversToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			string text = Emails.Email_Graze_Query();
			if (text.Length > 5)
			{
				MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				MessageBox.Show("Message and any attachments has been sent", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_0235: Unknown result type (might be due to invalid IL or missing references)
			//IL_023f: Expected O, but got Unknown
			//IL_0ebe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ec8: Expected O, but got Unknown
			components = new Container();
			lstResiduals = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			displayEventAgainstProfileToolStripMenuItem = new ToolStripMenuItem();
			menuStrip1 = new MenuStrip();
			withResidualsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyfullWidthToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byAxisAngleToolStripMenuItem = new ToolStripMenuItem();
			byObserverToolStripMenuItem = new ToolStripMenuItem();
			byObserverInitialToolStripMenuItem = new ToolStripMenuItem();
			byPAngleToolStripMenuItem = new ToolStripMenuItem();
			byPAToolStripMenuItem = new ToolStripMenuItem();
			byResidualToolStripMenuItem = new ToolStripMenuItem();
			bySequenceNumberToolStripMenuItem = new ToolStripMenuItem();
			byStarNumberToolStripMenuItem = new ToolStripMenuItem();
			byTimeToolStripMenuItem = new ToolStripMenuItem();
			grazeCoordinatorFunctionsToolStripMenuItem = new ToolStripMenuItem();
			sendEmailToOneOrMoreObserversToolStripMenuItem = new ToolStripMenuItem();
			mnuAddToLocalArchive = new ToolStripMenuItem();
			emailResultsAddToArchiveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			emailResultsonlyToolStripMenuItem = new ToolStripMenuItem();
			emailReductionsWithAMessageToolStripMenuItem = new ToolStripMenuItem();
			addToArchiveonlyToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			updateEmailAddressesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			cmdPlot = new Button();
			LabelRightClick = new Label();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstResiduals).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstResiduals).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResiduals).set_FormattingEnabled(true);
			lstResiduals.set_ItemHeight(14);
			((Control)lstResiduals).set_Location(new Point(8, 61));
			((Control)lstResiduals).set_Name("lstResiduals");
			((Control)lstResiduals).set_Size(new Size(1083, 648));
			((Control)lstResiduals).set_TabIndex(0);
			((Control)lstResiduals).add_MouseDown(new MouseEventHandler(lstResiduals_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)displayEventAgainstProfileToolStripMenuItem });
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(223, 26));
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Name("displayEventAgainstProfileToolStripMenuItem");
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).set_Text("Display event against profile");
			((ToolStripItem)displayEventAgainstProfileToolStripMenuItem).add_Click((EventHandler)displayEventAgainstProfileToolStripMenuItem_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withResidualsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)grazeCoordinatorFunctionsToolStripMenuItem,
				(ToolStripItem)mnuAddToLocalArchive,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1099, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withResidualsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyfullWidthToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withResidualsToolStripMenuItem).set_Name("withResidualsToolStripMenuItem");
			((ToolStripItem)withResidualsToolStripMenuItem).set_Size(new Size(115, 20));
			((ToolStripItem)withResidualsToolStripMenuItem).set_Text("with Residuals...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copyfullWidthToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyfullWidthToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyfullWidthToolStripMenuItem).set_Name("copyfullWidthToolStripMenuItem");
			((ToolStripItem)copyfullWidthToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)copyfullWidthToolStripMenuItem).set_Text("Copy (full width)");
			((ToolStripItem)copyfullWidthToolStripMenuItem).add_Click((EventHandler)copyfullWidthToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)byAxisAngleToolStripMenuItem,
				(ToolStripItem)byObserverToolStripMenuItem,
				(ToolStripItem)byObserverInitialToolStripMenuItem,
				(ToolStripItem)byPAngleToolStripMenuItem,
				(ToolStripItem)byPAToolStripMenuItem,
				(ToolStripItem)byResidualToolStripMenuItem,
				(ToolStripItem)bySequenceNumberToolStripMenuItem,
				(ToolStripItem)byStarNumberToolStripMenuItem,
				(ToolStripItem)byTimeToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(64, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort....    ");
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Name("byAxisAngleToolStripMenuItem");
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Text("by Axis Angle");
			((ToolStripItem)byAxisAngleToolStripMenuItem).add_Click((EventHandler)byAxisAngleToolStripMenuItem_Click);
			((ToolStripItem)byObserverToolStripMenuItem).set_Name("byObserverToolStripMenuItem");
			((ToolStripItem)byObserverToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byObserverToolStripMenuItem).set_Text("by Observer");
			((ToolStripItem)byObserverToolStripMenuItem).add_Click((EventHandler)byObserverToolStripMenuItem_Click);
			((ToolStripItem)byObserverInitialToolStripMenuItem).set_Name("byObserverInitialToolStripMenuItem");
			((ToolStripItem)byObserverInitialToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byObserverInitialToolStripMenuItem).set_Text("by Observer - exclude initial");
			((ToolStripItem)byObserverInitialToolStripMenuItem).add_Click((EventHandler)byObserverInitialToolStripMenuItem_Click);
			((ToolStripItem)byPAngleToolStripMenuItem).set_Name("byPAngleToolStripMenuItem");
			((ToolStripItem)byPAngleToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byPAngleToolStripMenuItem).set_Text("by P angle   [P,D librations]");
			((ToolStripItem)byPAngleToolStripMenuItem).add_Click((EventHandler)byPAngleToolStripMenuItem_Click);
			((ToolStripItem)byPAToolStripMenuItem).set_Name("byPAToolStripMenuItem");
			((ToolStripItem)byPAToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byPAToolStripMenuItem).set_Text("by P.A.");
			((ToolStripItem)byPAToolStripMenuItem).add_Click((EventHandler)byPAToolStripMenuItem_Click);
			((ToolStripItem)byResidualToolStripMenuItem).set_Name("byResidualToolStripMenuItem");
			((ToolStripItem)byResidualToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byResidualToolStripMenuItem).set_Text("by Residual");
			((ToolStripItem)byResidualToolStripMenuItem).add_Click((EventHandler)byResidualToolStripMenuItem_Click);
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Name("bySequenceNumberToolStripMenuItem");
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Text("by Sequence  number");
			((ToolStripItem)bySequenceNumberToolStripMenuItem).add_Click((EventHandler)bySequenceNumberToolStripMenuItem_Click);
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Name("byStarNumberToolStripMenuItem");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Text("by Star Number");
			((ToolStripItem)byStarNumberToolStripMenuItem).add_Click((EventHandler)byStarNumberToolStripMenuItem_Click);
			((ToolStripItem)byTimeToolStripMenuItem).set_Name("byTimeToolStripMenuItem");
			((ToolStripItem)byTimeToolStripMenuItem).set_Size(new Size(221, 22));
			((ToolStripItem)byTimeToolStripMenuItem).set_Text("by Time");
			((ToolStripItem)byTimeToolStripMenuItem).add_Click((EventHandler)byTimeToolStripMenuItem_Click);
			((ToolStripDropDownItem)grazeCoordinatorFunctionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem });
			((ToolStripItem)grazeCoordinatorFunctionsToolStripMenuItem).set_Name("grazeCoordinatorFunctionsToolStripMenuItem");
			((ToolStripItem)grazeCoordinatorFunctionsToolStripMenuItem).set_Size(new Size(192, 20));
			((ToolStripItem)grazeCoordinatorFunctionsToolStripMenuItem).set_Text("Graze organiser functions             ");
			((ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem).set_Image((Image)Resources.mail);
			((ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem).set_Name("sendEmailToOneOrMoreObserversToolStripMenuItem");
			((ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem).set_Size(new Size(267, 22));
			((ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem).set_Text("Send Email to one or more observers");
			((ToolStripItem)sendEmailToOneOrMoreObserversToolStripMenuItem).add_Click((EventHandler)sendEmailToOneOrMoreObserversToolStripMenuItem_Click);
			((ToolStripDropDownItem)mnuAddToLocalArchive).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)emailResultsAddToArchiveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)emailResultsonlyToolStripMenuItem,
				(ToolStripItem)emailReductionsWithAMessageToolStripMenuItem,
				(ToolStripItem)addToArchiveonlyToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)updateEmailAddressesToolStripMenuItem
			});
			((ToolStripItem)mnuAddToLocalArchive).set_Name("mnuAddToLocalArchive");
			((ToolStripItem)mnuAddToLocalArchive).set_Size(new Size(139, 20));
			((ToolStripItem)mnuAddToLocalArchive).set_Text("&Archive functions         ");
			((ToolStripItem)emailResultsAddToArchiveToolStripMenuItem).set_Name("emailResultsAddToArchiveToolStripMenuItem");
			((ToolStripItem)emailResultsAddToArchiveToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)emailResultsAddToArchiveToolStripMenuItem).set_Text("Email reductions, AND add observations to Archive");
			((ToolStripItem)emailResultsAddToArchiveToolStripMenuItem).add_Click((EventHandler)emailResultsAddToArchiveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(369, 6));
			((ToolStripItem)emailResultsonlyToolStripMenuItem).set_Name("emailResultsonlyToolStripMenuItem");
			((ToolStripItem)emailResultsonlyToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)emailResultsonlyToolStripMenuItem).set_Text("Email reductions - do not add to Archive");
			((ToolStripItem)emailResultsonlyToolStripMenuItem).add_Click((EventHandler)emailResultsonlyToolStripMenuItem_Click);
			((ToolStripItem)emailReductionsWithAMessageToolStripMenuItem).set_Name("emailReductionsWithAMessageToolStripMenuItem");
			((ToolStripItem)emailReductionsWithAMessageToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)emailReductionsWithAMessageToolStripMenuItem).set_Text("Email reductions with a message - do not add to Archive");
			((ToolStripItem)emailReductionsWithAMessageToolStripMenuItem).add_Click((EventHandler)emailReductionsWithAMessageToolStripMenuItem_Click);
			((ToolStripItem)addToArchiveonlyToolStripMenuItem).set_Name("addToArchiveonlyToolStripMenuItem");
			((ToolStripItem)addToArchiveonlyToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)addToArchiveonlyToolStripMenuItem).set_Text("Add observations to Archive (no Email sent)");
			((ToolStripItem)addToArchiveonlyToolStripMenuItem).add_Click((EventHandler)addToArchiveonlyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(369, 6));
			((ToolStripItem)updateEmailAddressesToolStripMenuItem).set_Name("updateEmailAddressesToolStripMenuItem");
			((ToolStripItem)updateEmailAddressesToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)updateEmailAddressesToolStripMenuItem).set_Text("Download current Email reporting addresses");
			((ToolStripItem)updateEmailAddressesToolStripMenuItem).add_Click((EventHandler)updateEmailAddressesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(105, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help               ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)cmdPlot).set_Anchor((AnchorStyles)1);
			((Control)cmdPlot).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlot).set_Location(new Point(453, 32));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(192, 24));
			((Control)cmdPlot).set_TabIndex(2);
			((Control)cmdPlot).set_Text("Plot graze events against profile");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			((Control)LabelRightClick).set_Anchor((AnchorStyles)1);
			((Control)LabelRightClick).set_AutoSize(true);
			((Control)LabelRightClick).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)LabelRightClick).set_Location(new Point(95, 46));
			((Control)LabelRightClick).set_Name("LabelRightClick");
			((Control)LabelRightClick).set_Size(new Size(201, 13));
			((Control)LabelRightClick).set_TabIndex(63);
			((Control)LabelRightClick).set_Text("Right-click on line to plot individual events");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1099, 715));
			((Control)this).get_Controls().Add((Control)(object)lstResiduals);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdPlot);
			((Control)this).get_Controls().Add((Control)(object)LabelRightClick);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationListResiduals", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationListResiduals);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1000, 200));
			((Control)this).set_Name("ListResiduals");
			((Control)this).set_Text("List of occultation residuals");
			((Form)this).add_Load((EventHandler)ListResiduals_Load);
			((Control)this).add_Resize((EventHandler)ListResiduals_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
