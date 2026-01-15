using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class SelectExportImportFile : Form
	{
		public string SelectedFile = "";

		private int NumFiles;

		private bool Set_NewFileName;

		private bool Cancelled;

		private bool matched;

		private CheckBox[] ChkBox;

		private Label[] Labels;

		private IContainer components;

		private Panel pnl1;

		private Label label1;

		private Button cmdClose;

		private Panel pnl2;

		private TextBox txtFileName;

		private Label lblExtension;

		private Label lblExisting;

		private Button cmdCancel;

		private TextBox txtEditor;

		private Label lblEditor;

		private Label lblFile;

		public SelectExportImportFile(string Files, bool Import, bool SetNewFileName)
		{
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_0157: Expected O, but got Unknown
			//IL_01d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			InitializeComponent();
			((Control)pnl2).set_Left(((Control)pnl1).get_Left());
			((Control)pnl1).set_Top(30);
			((Control)pnl2).set_Top(((Control)pnl1).get_Top() + 30);
			Set_NewFileName = SetNewFileName;
			if (Set_NewFileName)
			{
				((Control)pnl2).set_Visible(true);
				((Control)pnl1).set_Visible(false);
				((Control)label1).set_Text("Set a new Export file name");
			}
			else
			{
				TextBox obj = txtFileName;
				TextBox obj2 = txtEditor;
				Label obj3 = lblExisting;
				Label obj4 = lblEditor;
				Label obj5 = lblExtension;
				bool flag;
				((Control)lblFile).set_Visible(flag = false);
				bool flag2;
				((Control)obj5).set_Visible(flag2 = flag);
				bool flag3;
				((Control)obj4).set_Visible(flag3 = flag2);
				bool flag4;
				((Control)obj3).set_Visible(flag4 = flag3);
				bool visible;
				((Control)obj2).set_Visible(visible = flag4);
				((Control)obj).set_Visible(visible);
				if (Import)
				{
					((Control)label1).set_Text("Select file for Import");
				}
			}
			string[] array = Files.Split(new char[1] { ',' });
			NumFiles = array.Length;
			ChkBox = (CheckBox[])(object)new CheckBox[NumFiles];
			Labels = (Label[])(object)new Label[NumFiles];
			for (int i = 0; i < NumFiles; i++)
			{
				if (Set_NewFileName)
				{
					Labels[i] = new Label();
					((Control)Labels[i]).set_Location(new Point(50, 30 + 16 * i));
					((Control)Labels[i]).set_Text(array[i]);
					((Control)Labels[i]).set_Font(new Font("TimesNewRoman", 9f));
					((Control)Labels[i]).set_AutoSize(true);
					((Control)pnl2).get_Controls().Add((Control)(object)Labels[i]);
				}
				else
				{
					ChkBox[i] = new CheckBox();
					((Control)ChkBox[i]).set_Location(new Point(10, 5 + 18 * i));
					((Control)ChkBox[i]).set_Text(array[i]);
					ChkBox[i].set_Checked(false);
					((Control)ChkBox[i]).set_Font(new Font("TimesNewRoman", 9f));
					ChkBox[i].set_AutoCheck(false);
					((Control)ChkBox[i]).set_AutoSize(true);
					((Control)ChkBox[i]).add_Click((EventHandler)Checks);
					((Control)pnl1).get_Controls().Add((Control)(object)ChkBox[i]);
				}
			}
			string text = Settings.Default.ExportFile.ToUpper();
			if (Import)
			{
				text = Settings.Default.ImportFile.ToUpper();
			}
			bool flag5 = false;
			if (!Set_NewFileName)
			{
				for (int j = 0; j < NumFiles; j++)
				{
					if (((Control)ChkBox[j]).get_Text().ToUpper() == text)
					{
						ChkBox[j].set_Checked(true);
						flag5 = true;
						break;
					}
				}
				if (!flag5 & (NumFiles > 0))
				{
					ChkBox[0].set_Checked(true);
				}
			}
			if (Set_NewFileName)
			{
				((Control)txtEditor).Select();
			}
		}

		private void Checks(object sender, EventArgs e)
		{
			int tabIndex = ((Control)((sender is CheckBox) ? sender : null)).get_TabIndex();
			for (int i = 0; i < NumFiles; i++)
			{
				ChkBox[i].set_Checked(i == tabIndex);
			}
			Cancelled = false;
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void SelectExportImportFile_FormClosing(object sender, FormClosingEventArgs e)
		{
			Exit();
		}

		private void Exit()
		{
			if (Cancelled | matched)
			{
				SelectedFile = "";
			}
			else if (!Set_NewFileName)
			{
				for (int i = 0; i < NumFiles; i++)
				{
					if (ChkBox[i].get_Checked())
					{
						SelectedFile = ((Control)ChkBox[i]).get_Text();
						break;
					}
				}
			}
			if (Set_NewFileName & (SelectedFile != ""))
			{
				using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Import_Export\\" + SelectedFile))
				{
					streamWriter.WriteLine("<AsteroidExport>");
					streamWriter.WriteLine("  <Editor>" + ((Control)txtEditor).get_Text() + "</Editor>");
					streamWriter.WriteLine("</AsteroidExport>");
				}
			}
		}

		private void txtFileName_TextChanged(object sender, EventArgs e)
		{
			SelectedFile = ((Control)txtFileName).get_Text() + ((Control)lblExtension).get_Text();
			matched = false;
			for (int i = 0; i < NumFiles; i++)
			{
				if (((Control)Labels[i]).get_Text().ToUpper() == SelectedFile.ToUpper())
				{
					((Control)Labels[i]).set_ForeColor(Color.Red);
					matched = true;
				}
				else
				{
					((Control)Labels[i]).set_ForeColor(SystemColors.ControlText);
				}
			}
			if (matched)
			{
				((Control)txtFileName).set_ForeColor(Color.Red);
				((Control)lblExisting).set_Visible(true);
			}
			else
			{
				((Control)txtFileName).set_ForeColor(SystemColors.ControlText);
				((Control)lblExisting).set_Visible(false);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			Cancelled = true;
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdClose_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void txtEditor_Leave(object sender, EventArgs e)
		{
			//IL_003f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0045: Invalid comparison between Unknown and I4
			((Control)txtEditor).set_Text(((Control)txtEditor).get_Text().Trim());
			if (((Control)txtEditor).get_Text() == "")
			{
				if ((int)MessageBox.Show("The name of the Editor must be specified\r\n\r\nDo you want to Exit?", "Null name", (MessageBoxButtons)4, (MessageBoxIcon)16) == 6)
				{
					Cancelled = true;
					((Form)this).Close();
					((Component)this).Dispose();
				}
				else
				{
					((Control)txtEditor).Focus();
				}
			}
			string RevisedText = "";
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtEditor).get_Text(), "Name of Editor", CheckForPipes: true, out RevisedText))
			{
				((Control)txtEditor).set_Text(RevisedText);
				((Control)txtEditor).Focus();
			}
		}

		private void txtFileName_Leave(object sender, EventArgs e)
		{
			//IL_003f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0045: Invalid comparison between Unknown and I4
			((Control)txtFileName).set_Text(((Control)txtFileName).get_Text().Trim());
			if (((Control)txtFileName).get_Text() == "")
			{
				if ((int)MessageBox.Show("The file name must include characters before 'Export.xml'\r\n\r\nDo you want to Exit?", "Null name", (MessageBoxButtons)4, (MessageBoxIcon)16) == 6)
				{
					Cancelled = true;
					((Form)this).Close();
					((Component)this).Dispose();
				}
				else
				{
					((Control)txtFileName).Focus();
				}
			}
			string RevisedText = "";
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtFileName).get_Text(), "File name", CheckForPipes: true, out RevisedText))
			{
				((Control)txtFileName).set_Text(RevisedText);
				((Control)txtFileName).Focus();
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
			//IL_077b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0785: Expected O, but got Unknown
			pnl1 = new Panel();
			label1 = new Label();
			cmdClose = new Button();
			pnl2 = new Panel();
			txtFileName = new TextBox();
			lblExtension = new Label();
			lblExisting = new Label();
			cmdCancel = new Button();
			txtEditor = new TextBox();
			lblEditor = new Label();
			lblFile = new Label();
			((Control)this).SuspendLayout();
			((ScrollableControl)pnl1).set_AutoScroll(true);
			pnl1.set_AutoSizeMode((AutoSizeMode)0);
			((Control)pnl1).set_Location(new Point(4, 67));
			((Control)pnl1).set_Name("pnl1");
			((Control)pnl1).set_Size(new Size(355, 364));
			((Control)pnl1).set_TabIndex(10);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(7, 7));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(134, 17));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Select file for Export");
			cmdClose.set_DialogResult((DialogResult)1);
			((Control)cmdClose).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdClose).set_Location(new Point(271, 2));
			((Control)cmdClose).set_Name("cmdClose");
			((Control)cmdClose).set_Size(new Size(64, 27));
			((Control)cmdClose).set_TabIndex(3);
			((Control)cmdClose).set_Text("Accept");
			((ButtonBase)cmdClose).set_UseVisualStyleBackColor(true);
			((Control)cmdClose).add_Click((EventHandler)cmdClose_Click);
			((ScrollableControl)pnl2).set_AutoScroll(true);
			pnl2.set_AutoSizeMode((AutoSizeMode)0);
			((Control)pnl2).set_Location(new Point(7, 69));
			((Control)pnl2).set_Name("pnl2");
			((Control)pnl2).set_Size(new Size(355, 353));
			((Control)pnl2).set_TabIndex(4);
			((Control)pnl2).set_Visible(false);
			((Control)txtFileName).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFileName).set_Location(new Point(41, 58));
			((Control)txtFileName).set_Name("txtFileName");
			((Control)txtFileName).set_Size(new Size(129, 23));
			((Control)txtFileName).set_TabIndex(1);
			txtFileName.set_TextAlign((HorizontalAlignment)1);
			((Control)txtFileName).add_TextChanged((EventHandler)txtFileName_TextChanged);
			((Control)txtFileName).add_Leave((EventHandler)txtFileName_Leave);
			((Control)lblExtension).set_AutoSize(true);
			((Control)lblExtension).set_Font(new Font("Times New Roman", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblExtension).set_Location(new Point(176, 60));
			((Control)lblExtension).set_Name("lblExtension");
			((Control)lblExtension).set_Size(new Size(71, 16));
			((Control)lblExtension).set_TabIndex(8);
			((Control)lblExtension).set_Text("Export.xml");
			((Control)lblExisting).set_AutoSize(true);
			((Control)lblExisting).set_BackColor(Color.RoyalBlue);
			((Control)lblExisting).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblExisting).set_ForeColor(Color.Yellow);
			((Control)lblExisting).set_Location(new Point(250, 61));
			((Control)lblExisting).set_Name("lblExisting");
			((Control)lblExisting).set_Size(new Size(95, 17));
			((Control)lblExisting).set_TabIndex(9);
			((Control)lblExisting).set_Text("Existing name");
			((Control)lblExisting).set_Visible(false);
			cmdCancel.set_DialogResult((DialogResult)2);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(197, 2));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(64, 27));
			((Control)cmdCancel).set_TabIndex(2);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)txtEditor).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEditor).set_Location(new Point(109, 32));
			((Control)txtEditor).set_Name("txtEditor");
			((Control)txtEditor).set_Size(new Size(129, 23));
			((Control)txtEditor).set_TabIndex(0);
			((Control)txtEditor).add_Leave((EventHandler)txtEditor_Leave);
			((Control)lblEditor).set_AutoSize(true);
			((Control)lblEditor).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEditor).set_Location(new Point(5, 35));
			((Control)lblEditor).set_Name("lblEditor");
			((Control)lblEditor).set_Size(new Size(101, 17));
			((Control)lblEditor).set_TabIndex(6);
			((Control)lblEditor).set_Text("Name of editor");
			((Control)lblFile).set_AutoSize(true);
			((Control)lblFile).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFile).set_Location(new Point(7, 61));
			((Control)lblFile).set_Name("lblFile");
			((Control)lblFile).set_Size(new Size(30, 17));
			((Control)lblFile).set_TabIndex(7);
			((Control)lblFile).set_Text("File");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((ScrollableControl)this).set_AutoScroll(true);
			((ScrollableControl)this).set_AutoScrollMargin(new Size(0, 10));
			((Form)this).set_ClientSize(new Size(364, 442));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)lblFile);
			((Control)this).get_Controls().Add((Control)(object)lblEditor);
			((Control)this).get_Controls().Add((Control)(object)txtEditor);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)lblExisting);
			((Control)this).get_Controls().Add((Control)(object)lblExtension);
			((Control)this).get_Controls().Add((Control)(object)txtFileName);
			((Control)this).get_Controls().Add((Control)(object)pnl2);
			((Control)this).get_Controls().Add((Control)(object)cmdClose);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)pnl1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("SelectExportImportFile");
			((Control)this).set_Text("Select Export/Import file");
			((Form)this).add_FormClosing(new FormClosingEventHandler(SelectExportImportFile_FormClosing));
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
