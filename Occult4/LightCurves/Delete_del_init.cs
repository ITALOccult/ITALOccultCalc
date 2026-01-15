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
	public class Delete_del_init : Form
	{
		private IContainer components;

		private CheckedListBox chkLst_del_init;

		private Button cmdUncheck;

		private Button cmdDelete;

		private Button cmdChkAll;

		private Label label1;

		private Button cmdExit;

		public Delete_del_init()
		{
			InitializeComponent();
		}

		private void PopulateListOf_init_del_Files()
		{
			((ObjectCollection)chkLst_del_init.get_Items()).Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\LightCurveReports").GetFiles();
			if (files.Length < 0)
			{
				return;
			}
			FileInfo[] array = files;
			foreach (FileInfo fileInfo in array)
			{
				if (fileInfo.Extension == ".del")
				{
					((ObjectCollection)chkLst_del_init.get_Items()).Add((object)fileInfo.Name);
				}
			}
			array = files;
			foreach (FileInfo fileInfo2 in array)
			{
				if (fileInfo2.Extension == ".init")
				{
					((ObjectCollection)chkLst_del_init.get_Items()).Add((object)fileInfo2.Name);
				}
			}
		}

		private void Delete_del_init_Load(object sender, EventArgs e)
		{
			PopulateListOf_init_del_Files();
		}

		private void cmdChkAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkLst_del_init.get_Items()).get_Count(); i++)
			{
				chkLst_del_init.SetItemChecked(i, true);
			}
		}

		private void cmdUncheck_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkLst_del_init.get_Items()).get_Count(); i++)
			{
				chkLst_del_init.SetItemChecked(i, false);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0094: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d0: Invalid comparison between Unknown and I4
			string text = "";
			List<string> list = new List<string>();
			for (int i = 0; i < ((ObjectCollection)chkLst_del_init.get_Items()).get_Count(); i++)
			{
				if (chkLst_del_init.GetItemChecked(i))
				{
					list.Add(((ObjectCollection)chkLst_del_init.get_Items()).get_Item(i).ToString());
					text = text + "*    " + ((ObjectCollection)chkLst_del_init.get_Items()).get_Item(i).ToString() + "\r\n";
				}
			}
			if (list.Count < 1)
			{
				MessageBox.Show("No files selected for deletion", "No files selected", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			text = string.Format("Do you want to DELETE (to the recycle bin) \r\nthe following {0,1} files. \r\n", list.Count) + text;
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			for (int j = 0; j < list.Count; j++)
			{
				try
				{
					FileSystem.DeleteFile(Utilities.AppPath + "\\LightCurveReports\\" + list[j], (UIOption)2, (RecycleOption)3);
				}
				catch
				{
				}
			}
			PopulateListOf_init_del_Files();
			LightData.LightCurveView.ReReadSubmitted();
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Delete_del_init));
			chkLst_del_init = new CheckedListBox();
			cmdUncheck = new Button();
			cmdDelete = new Button();
			cmdChkAll = new Button();
			label1 = new Label();
			cmdExit = new Button();
			((Control)this).SuspendLayout();
			((Control)chkLst_del_init).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkLst_del_init).set_FormattingEnabled(true);
			((Control)chkLst_del_init).set_Location(new Point(6, 48));
			((Control)chkLst_del_init).set_Name("chkLst_del_init");
			((Control)chkLst_del_init).set_Size(new Size(356, 304));
			((Control)chkLst_del_init).set_TabIndex(0);
			((Control)cmdUncheck).set_Location(new Point(377, 113));
			((Control)cmdUncheck).set_Name("cmdUncheck");
			((Control)cmdUncheck).set_Size(new Size(88, 32));
			((Control)cmdUncheck).set_TabIndex(6);
			((Control)cmdUncheck).set_Text("Un-check All");
			((ButtonBase)cmdUncheck).set_UseVisualStyleBackColor(true);
			((Control)cmdUncheck).add_Click((EventHandler)cmdUncheck_Click);
			((Control)cmdDelete).set_Location(new Point(377, 167));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(88, 43));
			((Control)cmdDelete).set_TabIndex(5);
			((Control)cmdDelete).set_Text("Delete checked files");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)cmdChkAll).set_Location(new Point(377, 60));
			((Control)cmdChkAll).set_Name("cmdChkAll");
			((Control)cmdChkAll).set_Size(new Size(88, 32));
			((Control)cmdChkAll).set_TabIndex(4);
			((Control)cmdChkAll).set_Text("Check All");
			((ButtonBase)cmdChkAll).set_UseVisualStyleBackColor(true);
			((Control)cmdChkAll).add_Click((EventHandler)cmdChkAll_Click);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.Maroon);
			((Control)label1).set_Location(new Point(17, 10));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(391, 35));
			((Control)label1).set_TabIndex(7);
			((Control)label1).set_Text("This form deletes submitted light curve files marked as\r\n'.del' or '.init' .  [ They are moved to the recycle bin ]");
			((Control)cmdExit).set_Location(new Point(377, 309));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(88, 32));
			((Control)cmdExit).set_TabIndex(8);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(474, 358));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdUncheck);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Control)this).get_Controls().Add((Control)(object)cmdChkAll);
			((Control)this).get_Controls().Add((Control)(object)chkLst_del_init);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_Name("Delete_del_init");
			((Control)this).set_Text("Delete files marked .del & .init");
			((Form)this).add_Load((EventHandler)Delete_del_init_Load);
			((Control)this).ResumeLayout(false);
		}
	}
}
