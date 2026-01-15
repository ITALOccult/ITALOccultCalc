using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class AsteroidsInEditsFile : Form
	{
		private CheckBox[] ChkBox;

		private int NumEdits;

		private bool Cancelled;

		public int SelectedEntry;

		private IContainer components;

		private Panel panel1;

		private Button cmdOK;

		private Button cmdCancel;

		public AsteroidsInEditsFile()
		{
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009b: Expected O, but got Unknown
			InitializeComponent();
			NumEdits = AllEdits.All_Edits.Count;
			ChkBox = (CheckBox[])(object)new CheckBox[NumEdits];
			int num = Settings.Default.ImportFileLastEntry;
			if (num >= NumEdits)
			{
				num = 0;
			}
			int num2 = NumEdits;
			if (num2 < 5)
			{
				num2 = 5;
			}
			((Control)this).set_Height(100 + 22 * (num2 - 15));
			((Control)panel1).set_Height(5 + 22 * (num2 + 1));
			((Control)this).set_Height(((Control)panel1).get_Height() + 100);
			for (int i = 0; i < NumEdits; i++)
			{
				ChkBox[i] = new CheckBox();
				((Control)ChkBox[i]).set_Location(new Point(10, 5 + 20 * i + 10 * (i / 5)));
				((Control)ChkBox[i]).set_Text(AllEdits.All_Edits[i].EventID);
				ChkBox[i].set_Checked(i == num);
				((Control)ChkBox[i]).set_Font(new Font("Consolas", 9f));
				ChkBox[i].set_AutoCheck(false);
				((Control)ChkBox[i]).set_AutoSize(true);
				((Control)ChkBox[i]).add_Click((EventHandler)Checks);
				((Control)panel1).get_Controls().Add((Control)(object)ChkBox[i]);
			}
		}

		private void Checks(object sender, EventArgs e)
		{
			int tabIndex = ((Control)((sender is CheckBox) ? sender : null)).get_TabIndex();
			for (int i = 0; i < NumEdits; i++)
			{
				ChkBox[i].set_Checked(i == tabIndex);
			}
			Cancelled = false;
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void AsteroidsInEditsFile_FormClosing(object sender, FormClosingEventArgs e)
		{
			if (Cancelled)
			{
				SelectedEntry = -1;
				return;
			}
			for (int i = 0; i < NumEdits; i++)
			{
				if (ChkBox[i].get_Checked())
				{
					SelectedEntry = i;
					break;
				}
			}
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			Cancelled = false;
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			Cancelled = true;
			((Form)this).Close();
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
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			panel1 = new Panel();
			cmdOK = new Button();
			cmdCancel = new Button();
			((Control)this).SuspendLayout();
			((ScrollableControl)panel1).set_AutoScroll(true);
			panel1.set_AutoSizeMode((AutoSizeMode)0);
			((Control)panel1).set_Location(new Point(8, 46));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(304, 414));
			((Control)panel1).set_TabIndex(0);
			cmdOK.set_DialogResult((DialogResult)1);
			((Control)cmdOK).set_Location(new Point(104, 13));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(48, 24));
			((Control)cmdOK).set_TabIndex(1);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			cmdCancel.set_DialogResult((DialogResult)2);
			((Control)cmdCancel).set_Location(new Point(180, 13));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(53, 24));
			((Control)cmdCancel).set_TabIndex(2);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(316, 469));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("AsteroidsInEditsFile");
			((Control)this).set_Text("Select asteroid in Edit File");
			((Form)this).add_FormClosing(new FormClosingEventHandler(AsteroidsInEditsFile_FormClosing));
			((Control)this).ResumeLayout(false);
		}
	}
}
