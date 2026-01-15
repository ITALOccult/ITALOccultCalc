using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualBasic.FileIO;
using Occult;

namespace LightCurves
{
	public class Remove_Review_status : Form
	{
		private IContainer components;

		private CheckedListBox chkLstReview;

		private Button cmdChkAll;

		private Button cmdRemoveReview;

		private Button cmdUncheck;

		private Label label1;

		private Button cmdExit;

		public Remove_Review_status()
		{
			InitializeComponent();
		}

		private void PopulateListOf_Review_Files()
		{
			((ObjectCollection)chkLstReview.get_Items()).Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\LightCurveReports").GetFiles();
			if (files.Length < 0)
			{
				return;
			}
			FileInfo[] array = files;
			foreach (FileInfo fileInfo in array)
			{
				if (fileInfo.Extension == ".review")
				{
					((ObjectCollection)chkLstReview.get_Items()).Add((object)fileInfo.Name);
				}
			}
		}

		private void Remove_Review_status_Load(object sender, EventArgs e)
		{
			PopulateListOf_Review_Files();
		}

		private void cmdChkAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkLstReview.get_Items()).get_Count(); i++)
			{
				chkLstReview.SetItemChecked(i, true);
			}
		}

		private void cmdUncheck_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkLstReview.get_Items()).get_Count(); i++)
			{
				chkLstReview.SetItemChecked(i, false);
			}
		}

		private void cmdRemoveReview_Click(object sender, EventArgs e)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d0: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkLstReview.get_Items()).get_Count(); i++)
			{
				if (chkLstReview.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkLstReview.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkLstReview.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for Review status change", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = string.Format("Do you want to remove the Review status\r\nthe following {0,1} files. \r\n", list.Count) + text;
			if ((int)MessageBox.Show(text, "Confirm Review status change", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					FileSystem.RenameFile(Utilities.AppPath + "\\LightCurveReports\\" + list[j], list[j].Replace(".review", ""));
				}
				catch
				{
				}
			}
			PopulateListOf_Review_Files();
			LightData.LightCurveView.Read_ToReview_LightCurves(ShowMultiPlot: true);
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Remove_Review_status));
			chkLstReview = new CheckedListBox();
			cmdChkAll = new Button();
			cmdRemoveReview = new Button();
			cmdUncheck = new Button();
			label1 = new Label();
			cmdExit = new Button();
			((Control)this).SuspendLayout();
			((Control)chkLstReview).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkLstReview).set_FormattingEnabled(true);
			((Control)chkLstReview).set_Location(new Point(6, 48));
			((Control)chkLstReview).set_Name("chkLstReview");
			((Control)chkLstReview).set_Size(new Size(356, 304));
			((Control)chkLstReview).set_TabIndex(0);
			((Control)cmdChkAll).set_Location(new Point(377, 60));
			((Control)cmdChkAll).set_Name("cmdChkAll");
			((Control)cmdChkAll).set_Size(new Size(88, 32));
			((Control)cmdChkAll).set_TabIndex(1);
			((Control)cmdChkAll).set_Text("Check All");
			((ButtonBase)cmdChkAll).set_UseVisualStyleBackColor(true);
			((Control)cmdChkAll).add_Click((EventHandler)cmdChkAll_Click);
			((Control)cmdRemoveReview).set_Location(new Point(377, 167));
			((Control)cmdRemoveReview).set_Name("cmdRemoveReview");
			((Control)cmdRemoveReview).set_Size(new Size(88, 43));
			((Control)cmdRemoveReview).set_TabIndex(2);
			((Control)cmdRemoveReview).set_Text("Remove\r\nReview status");
			((ButtonBase)cmdRemoveReview).set_UseVisualStyleBackColor(true);
			((Control)cmdRemoveReview).add_Click((EventHandler)cmdRemoveReview_Click);
			((Control)cmdUncheck).set_Location(new Point(377, 113));
			((Control)cmdUncheck).set_Name("cmdUncheck");
			((Control)cmdUncheck).set_Size(new Size(88, 32));
			((Control)cmdUncheck).set_TabIndex(3);
			((Control)cmdUncheck).set_Text("Un-check All");
			((ButtonBase)cmdUncheck).set_UseVisualStyleBackColor(true);
			((Control)cmdUncheck).add_Click((EventHandler)cmdUncheck_Click);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.Maroon);
			((Control)label1).set_Location(new Point(8, 9));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(367, 35));
			((Control)label1).set_TabIndex(8);
			((Control)label1).set_Text("This form removes the 'Review' status of  submitted\r\nlight curve files marked as '.review'");
			((Control)cmdExit).set_Location(new Point(377, 309));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(88, 32));
			((Control)cmdExit).set_TabIndex(9);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(474, 358));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdUncheck);
			((Control)this).get_Controls().Add((Control)(object)cmdRemoveReview);
			((Control)this).get_Controls().Add((Control)(object)cmdChkAll);
			((Control)this).get_Controls().Add((Control)(object)chkLstReview);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_Name("Remove_Review_status");
			((Control)this).set_Text("Remove review status");
			((Form)this).add_Load((EventHandler)Remove_Review_status_Load);
			((Control)this).ResumeLayout(false);
		}
	}
}
