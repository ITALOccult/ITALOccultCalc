using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Occult.Star_Catalogues
{
	internal class EditXZ_Forms
	{
		internal static XZVariables_Editor VariableEditor;

		internal static XZDoubles_Editor DoubleEditor;

		internal static List<XZVariables> VariableList = new List<XZVariables>();

		internal static List<XZDoubles> DoubleList = new List<XZDoubles>();

		internal static List<OCCstars> OCCList = new List<OCCstars>();

		internal static void Show_VariableEditor()
		{
			try
			{
				((Control)VariableEditor).Show();
			}
			catch
			{
				VariableEditor = new XZVariables_Editor();
				((Control)VariableEditor).Show();
			}
			((Control)VariableEditor).Focus();
		}

		internal static void Show_DoubleEditor()
		{
			try
			{
				((Control)DoubleEditor).Show();
			}
			catch
			{
				DoubleEditor = new XZDoubles_Editor();
				((Control)DoubleEditor).Show();
			}
			((Control)DoubleEditor).Focus();
		}

		internal static void ReadVariableStarFile()
		{
			VariableList.Clear();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZVariables.dat");
			do
			{
				XZVariables xZVariables = new XZVariables();
				xZVariables.DecodeLine(streamReader.ReadLine());
				VariableList.Add(xZVariables);
			}
			while (!streamReader.EndOfStream);
		}

		internal static void WriteVariableStarFile()
		{
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\XZVariables.dat");
			for (int i = 0; i < VariableList.Count; i++)
			{
				streamWriter.WriteLine(VariableList[i].ToString());
			}
		}

		internal static void ReadDoubleStarFile()
		{
			DoubleList.Clear();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat");
			do
			{
				XZDoubles xZDoubles = new XZDoubles();
				xZDoubles.DecodeLine(streamReader.ReadLine());
				DoubleList.Add(xZDoubles);
			}
			while (!streamReader.EndOfStream);
		}

		internal static void WriteDoubleStarFile()
		{
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\XZDoubles.dat");
			for (int i = 0; i < DoubleList.Count; i++)
			{
				streamWriter.WriteLine(DoubleList[i].SaveString());
			}
		}

		internal static void ReadOCCStarFile()
		{
			OCCList.Clear();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\XZDoubles Discoveries.dat");
			do
			{
				OCCstars oCCstars = new OCCstars();
				oCCstars.DecodeLine(streamReader.ReadLine());
				OCCList.Add(oCCstars);
			}
			while (!streamReader.EndOfStream);
		}

		internal static void WriteOCCStarFile()
		{
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\XZDoubles Discoveries.dat");
			for (int i = 0; i < OCCList.Count; i++)
			{
				streamWriter.WriteLine(OCCList[i].ToString());
			}
		}
	}
}
