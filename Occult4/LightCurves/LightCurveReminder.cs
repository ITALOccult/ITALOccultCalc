using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Occult;
using Occult.File_Actions;

namespace LightCurves
{
	public class LightCurveReminder : Form
	{
		private List<FileNameDate> LCfiles;

		private FileNameDate FileNameDate;

		private static long DownloadLength;

		private IContainer components;

		private Label label1;

		internal RadioButton optNow;

		internal RadioButton optMyEmail;

		internal RadioButton optLater;

		private Button cmdOK;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Panel panel1;

		private Label label2;

		private Panel pnlDelete;

		private CheckedListBox chkDownload;

		private Button cmdClearAll;

		private Button cmdCheckAll;

		private Button cmdMove;

		private Button cmdDelete;

		private Label label13;

		private Button cmdCopy;

		private Panel panel2;

		public LightCurveReminder(int LCcount)
		{
			InitializeComponent();
			((Control)label1).set_Text("You have " + LCcount + " light curves that are due to be uploaded");
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			if (optNow.get_Checked())
			{
				((Form)this).set_DialogResult((DialogResult)1);
			}
			else
			{
				((Form)this).set_DialogResult((DialogResult)7);
			}
		}

		private void LightCurveReminder_Load(object sender, EventArgs e)
		{
			PopulateCheckList();
		}

		private void PopulateCheckList()
		{
			((ObjectCollection)chkDownload.get_Items()).Clear();
			string path = Utilities.AppPath + "\\Observations\\LightCurves\\";
			LCfiles = new List<FileNameDate>();
			List<FileInfo> list = (from f in new DirectoryInfo(path).GetFiles("*.dat")
				orderby f.LastWriteTime
				select f).ToList();
			for (int i = 0; i < list.Count; i++)
			{
				FileNameDate = new FileNameDate();
				FileNameDate.FName = Path.GetFileName(list[i].Name);
				FileNameDate.Date = list[i].LastWriteTime;
				FileNameDate.Size = list[i].Length;
				LCfiles.Add(FileNameDate);
			}
			LCfiles.Sort();
			((ObjectCollection)chkDownload.get_Items()).Clear();
			for (int j = 0; j < LCfiles.Count; j++)
			{
				((ObjectCollection)chkDownload.get_Items()).Add((object)LCfiles[j].FName.PadLeft(30));
				DownloadLength += LCfiles[j].Size;
			}
		}

		private void cmdCheckAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				chkDownload.SetItemChecked(i, true);
			}
		}

		private void cmdClearAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				chkDownload.SetItemChecked(i, false);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			MoveDeleteDownloads(Delete: true);
		}

		private void cmdMove_Click(object sender, EventArgs e)
		{
			MoveDeleteDownloads(Delete: false);
		}

		private void MoveDeleteDownloads(bool Delete)
		{
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Invalid comparison between Unknown and I4
			string text = Utilities.AppPath + "\\Observations\\LightCurves";
			string text2 = text + "\\Reported";
			string text3 = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				if (chkDownload.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString()!.Trim());
					text3 = text3 + "*    " + ((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text3 = ((!Delete) ? (string.Format("Do you want to move the following {0,1} files to the " + text2 + " folder:\r\n", list.Count) + text3) : (string.Format("Do you want to permanently DELETE the following {0,1} files. \r\nThey will not be recoverable from the Recycle Bin:\r\n", list.Count) + text3));
			if ((int)MessageBox.Show(text3, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(text2))
			{
				Directory.CreateDirectory(text2);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					if (Delete)
					{
						File.Delete(text + "\\" + list[j]);
					}
					else
					{
						File.Move(text + "\\" + list[j], text2 + "\\" + list[j]);
					}
				}
				catch
				{
				}
			}
			PopulateCheckList();
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			//IL_00c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0101: Unknown result type (might be due to invalid IL or missing references)
			//IL_0107: Invalid comparison between Unknown and I4
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.AppPath + "\\Observations\\LightCurves";
			string text2 = Utilities.AppPath + "\\LightCurveReports";
			string text3 = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkDownload.get_Items()).get_Count(); i++)
			{
				if (chkDownload.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString()!.Trim());
					text3 = text3 + " *    " + ((ObjectCollection)chkDownload.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text3 = string.Format("Do you want to copy the following {0,1} files to the " + text2 + " folder:\r\n", list.Count) + text3;
			if ((int)MessageBox.Show(text3, "Confirm copy", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (!Directory.Exists(text2))
			{
				Directory.CreateDirectory(text2);
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					File.Copy(text + "\\" + list[j], text2 + "\\" + list[j]);
				}
				catch (Exception ex)
				{
					MessageBox.Show("File copy failed: " + list[j] + "\r\n" + ex.Message);
				}
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			label1 = new Label();
			optNow = new RadioButton();
			optMyEmail = new RadioButton();
			optLater = new RadioButton();
			cmdOK = new Button();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			panel1 = new Panel();
			label2 = new Label();
			pnlDelete = new Panel();
			cmdMove = new Button();
			cmdDelete = new Button();
			cmdClearAll = new Button();
			cmdCheckAll = new Button();
			chkDownload = new CheckedListBox();
			label13 = new Label();
			cmdCopy = new Button();
			panel2 = new Panel();
			((Control)panel1).SuspendLayout();
			((Control)pnlDelete).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(6, 13));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(308, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("You have 0 light curves that are due to be uploaded.");
			((Control)optNow).set_AutoSize(true);
			((Control)optNow).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNow).set_ForeColor(Color.DarkGreen);
			((Control)optNow).set_Location(new Point(6, 61));
			((Control)optNow).set_Name("optNow");
			((Control)optNow).set_Size(new Size(327, 17));
			((Control)optNow).set_TabIndex(5);
			((Control)optNow).set_Text("I want to upload the files now, using the Occult email");
			((ButtonBase)optNow).set_UseVisualStyleBackColor(true);
			((Control)optMyEmail).set_AutoSize(true);
			((Control)optMyEmail).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMyEmail).set_ForeColor(Color.DarkBlue);
			((Control)optMyEmail).set_Location(new Point(6, 100));
			((Control)optMyEmail).set_Name("optMyEmail");
			((Control)optMyEmail).set_Size(new Size(315, 17));
			((Control)optMyEmail).set_TabIndex(6);
			((Control)optMyEmail).set_Text("I will manually upload the files using my usual email");
			((ButtonBase)optMyEmail).set_UseVisualStyleBackColor(true);
			((Control)optLater).set_AutoSize(true);
			optLater.set_Checked(true);
			((Control)optLater).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optLater).set_ForeColor(Color.DarkRed);
			((Control)optLater).set_Location(new Point(6, 33));
			((Control)optLater).set_Name("optLater");
			((Control)optLater).set_Size(new Size(250, 17));
			((Control)optLater).set_TabIndex(7);
			optLater.set_TabStop(true);
			((Control)optLater).set_Text("I want to upload the files at a later date");
			((ButtonBase)optLater).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOK).set_Location(new Point(134, 125));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(70, 26));
			((Control)cmdOK).set_TabIndex(8);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.DarkRed);
			((Control)label3).set_Location(new Point(89, 4));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(158, 17));
			((Control)label3).set_TabIndex(9);
			((Control)label3).set_Text("Note for GMail users");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(11, 27));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(279, 26));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("If you are using gmail for your emails, uploading via Occult\r\nwill probably not work. Send a 'normal' email to");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(58, 56));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(144, 13));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("heraldDR@bigpond.com");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(11, 78));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(165, 13));
			((Control)label6).set_TabIndex(12);
			((Control)label6).set_Text("with the subject line set exactly to");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_ForeColor(Color.Red);
			((Control)label7).set_Location(new Point(58, 132));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(208, 13));
			((Control)label7).set_TabIndex(13);
			((Control)label7).set_Text("Occult 4/Observations/LightCurves");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_ForeColor(Color.DarkBlue);
			((Control)label8).set_Location(new Point(58, 94));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(108, 13));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("Light curve report");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(11, 116));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(273, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("You will find the light curve files to attach to the email, in ");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(11, 154));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(315, 13));
			((Control)label10).set_TabIndex(16);
			((Control)label10).set_Text("After emailing the light curves, move them to a subdirectory called");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.Red);
			((Control)label11).set_Location(new Point(58, 170));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(266, 13));
			((Control)label11).set_TabIndex(17);
			((Control)label11).set_Text("Occult 4/Observations/LightCurves/Reported");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_ForeColor(Color.DarkGreen);
			((Control)label12).set_Location(new Point(11, 192));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(305, 26));
			((Control)label12).set_TabIndex(18);
			((Control)label12).set_Text("(If necessary, create it.) \r\nThis will prevent duplicate submissions, as well as this reminder!");
			((Control)panel1).set_BackColor(Color.PapayaWhip);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)label11);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).set_Location(new Point(0, 162));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(338, 233));
			((Control)panel1).set_TabIndex(19);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_ForeColor(Color.DarkMagenta);
			((Control)label2).set_Location(new Point(5, 77));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(329, 13));
			((Control)label2).set_TabIndex(20);
			((Control)label2).set_Text("To set your email details:  Maintenance  tab, User settings, Group 4, ");
			((Control)pnlDelete).set_BackColor(Color.Honeydew);
			pnlDelete.set_BorderStyle((BorderStyle)2);
			((Control)pnlDelete).get_Controls().Add((Control)(object)panel2);
			((Control)pnlDelete).get_Controls().Add((Control)(object)cmdClearAll);
			((Control)pnlDelete).get_Controls().Add((Control)(object)cmdCheckAll);
			((Control)pnlDelete).get_Controls().Add((Control)(object)chkDownload);
			((Control)pnlDelete).set_Location(new Point(359, 50));
			((Control)pnlDelete).set_Name("pnlDelete");
			((Control)pnlDelete).set_Size(new Size(256, 344));
			((Control)pnlDelete).set_TabIndex(21);
			((Control)cmdMove).set_BackColor(Color.Cornsilk);
			((Control)cmdMove).set_Location(new Point(103, 4));
			((Control)cmdMove).set_Name("cmdMove");
			((Control)cmdMove).set_Size(new Size(63, 61));
			((Control)cmdMove).set_TabIndex(42);
			((Control)cmdMove).set_Text("Move checked files to 'Reported'");
			((ButtonBase)cmdMove).set_UseVisualStyleBackColor(false);
			((Control)cmdMove).add_Click((EventHandler)cmdMove_Click);
			((Control)cmdDelete).set_BackColor(Color.LavenderBlush);
			((Control)cmdDelete).set_Location(new Point(183, 4));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(63, 61));
			((Control)cmdDelete).set_TabIndex(41);
			((Control)cmdDelete).set_Text("Delete checked files");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(false);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)cmdClearAll).set_BackColor(Color.Aquamarine);
			((Control)cmdClearAll).set_Location(new Point(148, 80));
			((Control)cmdClearAll).set_Name("cmdClearAll");
			((Control)cmdClearAll).set_Size(new Size(78, 24));
			((Control)cmdClearAll).set_TabIndex(40);
			((Control)cmdClearAll).set_Text("Uncheck  all");
			((ButtonBase)cmdClearAll).set_UseVisualStyleBackColor(false);
			((Control)cmdClearAll).add_Click((EventHandler)cmdClearAll_Click);
			((Control)cmdCheckAll).set_BackColor(Color.MediumSpringGreen);
			((Control)cmdCheckAll).set_Location(new Point(26, 80));
			((Control)cmdCheckAll).set_Name("cmdCheckAll");
			((Control)cmdCheckAll).set_Size(new Size(78, 24));
			((Control)cmdCheckAll).set_TabIndex(39);
			((Control)cmdCheckAll).set_Text("Check  all");
			((ButtonBase)cmdCheckAll).set_UseVisualStyleBackColor(false);
			((Control)cmdCheckAll).add_Click((EventHandler)cmdCheckAll_Click);
			((Control)chkDownload).set_BackColor(Color.LightYellow);
			((Control)chkDownload).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkDownload).set_FormattingEnabled(true);
			((Control)chkDownload).set_Location(new Point(-1, 110));
			((Control)chkDownload).set_Name("chkDownload");
			((Control)chkDownload).set_Size(new Size(254, 229));
			((Control)chkDownload).set_TabIndex(37);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.Firebrick);
			((Control)label13).set_Location(new Point(367, 6));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(241, 40));
			((Control)label13).set_TabIndex(22);
			((Control)label13).set_Text("- OR -\r\nMove or Delete selected files");
			label13.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdCopy).set_BackColor(Color.Cornsilk);
			((Control)cmdCopy).set_Location(new Point(3, 4));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(83, 61));
			((Control)cmdCopy).set_TabIndex(43);
			((Control)cmdCopy).set_Text("Copy checked files to 'Light Curve Reports'");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)panel2).set_BackColor(Color.Yellow);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdCopy);
			((Control)panel2).get_Controls().Add((Control)(object)cmdMove);
			((Control)panel2).get_Controls().Add((Control)(object)cmdDelete);
			((Control)panel2).set_Location(new Point(0, 1));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(250, 71));
			((Control)panel2).set_TabIndex(44);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(626, 397));
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)pnlDelete);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)optLater);
			((Control)this).get_Controls().Add((Control)(object)optMyEmail);
			((Control)this).get_Controls().Add((Control)(object)optNow);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Control)this).set_Name("LightCurveReminder");
			((Control)this).set_Text("Light curves due for upload");
			((Form)this).add_Load((EventHandler)LightCurveReminder_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)pnlDelete).ResumeLayout(false);
			((Control)panel2).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
