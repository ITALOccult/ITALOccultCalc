using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class Unistellar : Form
	{
		private string eVpath = "c:\\";

		private string[] SubdirectoryNames = new string[3] { "EventsToAdd", "ObservationsToAdd", "EventsToIgnore" };

		private string[] Subdirectories = new string[3];

		private static AllEvents eVfile;

		private static Unistellar_CopyObservations CopyObservations;

		private IContainer components;

		private TextBox textBox1;

		private Button cmdSetDirectory;

		private TextBox txtDirectory;

		private Button cmdSort;

		private Button cmdProcessExistingEvents;

		private Panel panel1;

		public Unistellar()
		{
			InitializeComponent();
			((TextBoxBase)textBox1).set_SelectionStart(0);
			((TextBoxBase)textBox1).set_SelectionLength(0);
			string unistellar_Directory;
			((Control)txtDirectory).set_Text(unistellar_Directory = Settings.Default.Unistellar_Directory);
			eVpath = unistellar_Directory;
		}

		private void Unistellar_Load(object sender, EventArgs e)
		{
			SetSubdirectoryNames();
		}

		private void cmdSetDirectory_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_003f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0045: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_Description("Specify the directory containing the Unistellar observations to sort");
			val.set_SelectedPath(Settings.Default.Unistellar_Directory);
			if (val.get_SelectedPath() == "")
			{
				val.set_SelectedPath(Utilities.AppPath);
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.Unistellar_Directory = val.get_SelectedPath();
				((Control)txtDirectory).set_Text(val.get_SelectedPath());
				eVpath = val.get_SelectedPath();
				SetSubdirectoryNames();
			}
		}

		private void SetSubdirectoryNames()
		{
			for (int i = 0; i < 3; i++)
			{
				Subdirectories[i] = eVpath + "\\" + SubdirectoryNames[i];
			}
		}

		private void cmdSort_Click(object sender, EventArgs e)
		{
			//IL_031c: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			string text = "";
			int result = 0;
			int result2 = 1;
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			DateTime dateTime = DateTime.Now.AddDays(-90.0);
			for (int i = 0; i < 3; i++)
			{
				if (!Directory.Exists(Subdirectories[i]))
				{
					Directory.CreateDirectory(Subdirectories[i]);
				}
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			HistoricalIndexData.SortField = 2;
			Asteroid_Observations_Reports.HistoricalIndex.Sort();
			string[] files = Directory.GetFiles(eVpath, "*.xml");
			foreach (string text2 in files)
			{
				flag = false;
				eVfile = new AllEvents();
				eVfile.ReadObservationsFile(HistoricalFile: false, text2);
				string indexLine = eVfile.LinesOfAnEvent.IndexLine;
				string text3 = indexLine.Substring(0, 10);
				int.TryParse(indexLine.Substring(11, 6).Trim(), out result2);
				text = indexLine.Substring(0, 4) + indexLine.Substring(5, 2).Trim().PadLeft(2, '0') + indexLine.Substring(8, 2).Trim().PadLeft(2, '0') + "_" + result2 + "_" + indexLine.Substring(18, 13).Trim() + ".xml";
				int.TryParse(indexLine.Substring(36, 3), out result);
				for (int k = 0; k < Data_and_Plots.Historical_AllEvents.OccEvents.Count; k++)
				{
					string text4 = Data_and_Plots.Historical_AllEvents.OccEvents[k].IndexLine.Substring(0, 10);
					int astNum = Data_and_Plots.Historical_AllEvents.OccEvents[k].AstNum;
					if (text4 == text3 && astNum == result2)
					{
						flag = true;
						num2++;
						eVfile.WriteHistoricalObservationsFile(HistoricalFile: false, Subdirectories[1] + "\\" + text);
						File.Delete(text2);
						break;
					}
				}
				if (flag)
				{
					continue;
				}
				eVfile.GetEventDate(0, out var Year, out var Month, out var Day);
				if (new DateTime(Year, Month, Day) < dateTime)
				{
					if (result > 0)
					{
						eVfile.WriteHistoricalObservationsFile(HistoricalFile: false, Subdirectories[0] + "\\" + text);
						num++;
					}
					else
					{
						eVfile.WriteHistoricalObservationsFile(HistoricalFile: false, Subdirectories[2] + "\\" + text);
						num3++;
					}
					File.Delete(text2);
				}
				else
				{
					num4++;
				}
			}
			MessageBox.Show("The result of the Unistellar event sort is:\r\n- Events to be added = " + num + "\r\n- Observations to be added = " + num2 + "\r\n- Events to be ignored = " + num3 + "\r\n- Events not yet processed = " + num4 + "\r\n\r\nThe data in those files should now be added to the Historical Observations file\r\n- Events to be added should be added as new events\r\n- Observations to be added - open the file in a text editor, and\r\n   paste the groups of lines within the tag <Observer> into \r\n   the relevant Historical event in occult", "Unistellar sort summary", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdProcessExistingEvents_Click(object sender, EventArgs e)
		{
			//IL_002f: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				if (!((Control)CopyObservations).get_Visible())
				{
					((Control)CopyObservations).Show();
				}
				else
				{
					((Control)CopyObservations).BringToFront();
				}
				MessageBox.Show("The form for processing existing events is already open, and must be closed, as\r\n\r\nthe open form must be closed before this function can be used.\r\n\r\nThe form will now be closed", "Close form", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Form)CopyObservations).Close();
				((Component)(object)CopyObservations).Dispose();
			}
			catch
			{
			}
			CopyObservations = new Unistellar_CopyObservations(Subdirectories[1]);
			try
			{
				((Control)CopyObservations).Show();
			}
			catch
			{
			}
		}

		private void Unistellar_HelpButtonClicked(object sender, CancelEventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Unistellar");
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Unistellar));
			textBox1 = new TextBox();
			cmdSetDirectory = new Button();
			txtDirectory = new TextBox();
			cmdSort = new Button();
			cmdProcessExistingEvents = new Button();
			panel1 = new Panel();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)textBox1).set_BackColor(Color.OldLace);
			((Control)textBox1).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox1).set_Location(new Point(-3, -1));
			((TextBoxBase)textBox1).set_Multiline(true);
			((Control)textBox1).set_Name("textBox1");
			((TextBoxBase)textBox1).set_ReadOnly(true);
			((Control)textBox1).set_Size(new Size(810, 308));
			((Control)textBox1).set_TabIndex(0);
			((Control)textBox1).set_Text(componentResourceManager.GetString("textBox1.Text"));
			((Control)cmdSetDirectory).set_Location(new Point(90, 7));
			((Control)cmdSetDirectory).set_Name("cmdSetDirectory");
			((Control)cmdSetDirectory).set_Size(new Size(158, 48));
			((Control)cmdSetDirectory).set_TabIndex(1);
			((Control)cmdSetDirectory).set_Text("Select the directory that\r\ncontains the  eVscope reports");
			((ButtonBase)cmdSetDirectory).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDirectory).add_Click((EventHandler)cmdSetDirectory_Click);
			((Control)txtDirectory).set_Location(new Point(10, 61));
			((Control)txtDirectory).set_Name("txtDirectory");
			((TextBoxBase)txtDirectory).set_ReadOnly(true);
			((Control)txtDirectory).set_Size(new Size(318, 20));
			((Control)txtDirectory).set_TabIndex(3);
			((Control)txtDirectory).set_Text("...Occult4\\Asteroid\\Observations\\2021\\eVscope");
			((Control)cmdSort).set_Location(new Point(391, 317));
			((Control)cmdSort).set_Name("cmdSort");
			((Control)cmdSort).set_Size(new Size(158, 66));
			((Control)cmdSort).set_TabIndex(4);
			((Control)cmdSort).set_Text("Sort the eVscope reports into\r\n* new events                     .\r\n* data for existing events   .\r\n* Miss reports of no value  .");
			((ButtonBase)cmdSort).set_UseVisualStyleBackColor(true);
			((Control)cmdSort).add_Click((EventHandler)cmdSort_Click);
			((Control)cmdProcessExistingEvents).set_Location(new Point(601, 326));
			((Control)cmdProcessExistingEvents).set_Name("cmdProcessExistingEvents");
			((Control)cmdProcessExistingEvents).set_Size(new Size(158, 48));
			((Control)cmdProcessExistingEvents).set_TabIndex(5);
			((Control)cmdProcessExistingEvents).set_Text("Process the files for\r\n'Existing Events' ");
			((ButtonBase)cmdProcessExistingEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessExistingEvents).add_Click((EventHandler)cmdProcessExistingEvents_Click);
			((Control)panel1).set_BackColor(Color.LightCyan);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)txtDirectory);
			((Control)panel1).get_Controls().Add((Control)(object)cmdSetDirectory);
			((Control)panel1).set_Location(new Point(-1, 304));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(340, 89));
			((Control)panel1).set_TabIndex(6);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.Honeydew);
			((Form)this).set_ClientSize(new Size(803, 392));
			((Control)this).get_Controls().Add((Control)(object)cmdProcessExistingEvents);
			((Control)this).get_Controls().Add((Control)(object)cmdSort);
			((Control)this).get_Controls().Add((Control)(object)textBox1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_HelpButton(true);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("eVscope");
			((Control)this).set_Text("eVscope - Identify new events, and observations to be added to existing events");
			((Form)this).add_HelpButtonClicked((CancelEventHandler)Unistellar_HelpButtonClicked);
			((Form)this).add_Load((EventHandler)Unistellar_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
