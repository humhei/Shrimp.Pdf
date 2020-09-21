1.  Replace contents in  Shrimp.pdf.fsproj `Never` to `PreserveNewest`
```
    <None Include="Resources\Marks\*.pdf">
      <CopyToOutputDirectory>Never</CopyToOutputDirectory>
    </None>
    <None Include="Resources\Fonts\*.otf">
      <CopyToOutputDirectory>Never</CopyToOutputDirectory>
    </None>

    <None Include="Resources\Colors\*.pdf">
      <CopyToOutputDirectory>Never</CopyToOutputDirectory>
    </None>
    <None Include="Resources\Colors\*\*.pdf">
      <CopyToOutputDirectory>Never</CopyToOutputDirectory>
    </None>

    <None Update="Resources\Colors\registion.pdf">
      <CopyToOutputDirectory>Never</CopyToOutputDirectory>
    </None>
```
2. Multiple startup projects
`Shrimp.Pdf.icms2.Server`
`Shrimp.Pdf.Tests`

3. Press F5 In vs2019
